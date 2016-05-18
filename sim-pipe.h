#include "machine.h"

/* define enumerate type for ALU operation */
enum alu_op {
	ALU_ADD, ALU_SUB, ALU_AND, ALU_OR, ALU_SLT, ALU_SHL, ALU_MULTU
};

/* define branch signal type */
enum branch_sig {
	B_NONE, B_J, B_NE, B_EQ
};

/* deifine values of signal */
typedef struct{
	unsigned int aluop:3;
	unsigned int alusrc:1;
	unsigned int regwrite:1;
	unsigned int regdst:1;
	unsigned int branch:22;
	unsigned int memwrite:1;
	unsigned int memread:1;
	unsigned int memtoreg:1;
}signal_t;


/* define values related to operands, all possible combinations are included */
typedef struct{
  int in1;			/* input 1 register number */
  int in2;			/* input 2 register number */
  int in3;			/* input 3 register number */
  int out1;			/* output 1 register number */
  int out2;			/* output 2 register number */
 // ......
}oprand_t;


/*define buffer between fetch and decode stage*/
struct ifid_buf {
  md_inst_t inst;	    /* instruction that has been fetched */
  md_addr_t PC;	        /* pc value of current instruction */
  md_addr_t NPC;		/* the next instruction to fetch */
	int IFstop;				/* the signal control the end of instruction fetch */
};


/*define buffer between decode and execute stage*/
struct idex_buf {
  md_addr_t PC;	        /* pc value of current instruction */
  md_inst_t inst;		/* instruction in ID stage */ 
	int latched;
	int instFlags;
  int opcode;			/* operation number */
  oprand_t oprand;		/* operand */
	signal_t signal;		/* signal */
	word_t reg_data_1;
	word_t reg_data_2;
	word_t imm;
	int reg_dst_rt;			/* candidate destination register rt */
	int reg_dst_rd;			/* candidate destination register rd */
  //......  
};

/*define buffer between execute and memory stage*/
struct exmem_buf{
	md_addr_t TPC;		/* targe pc value of branch instruction */
	md_addr_t PC;			/* pc value of current instruction */
  md_inst_t inst;		/* instruction in EX stage */
  int opcode;			/* operation number */
	signal_t signal;			/* signal */
	int PCsrc;						/* signal to control if NPC is replaced by TPC */
	int zero;				/* set to 1 when output of main alu is zero */
	word_t ALUresult;		/* the result of alu computation */
	word_t reg_to_mem;	/* the data to be write to memory */
	int regDst;					/* destination register number, chosen between rt and rd */
  //......
};

/*define buffer between memory and writeback stage*/
struct memwb_buf{
	md_addr_t PC; 				/* pc value of current instruction */
  md_inst_t inst;		/* instruction in MEM stage */
	md_addr_t dumped_PC;			/* pc value that comes out of the pipeline */
	md_inst_t dumped_inst;		/* instruction that comes out of the pipeline */
	signal_t signal;			/* signal */
	word_t mem_data;			/* data read from memory */
	word_t alu_data;			/* data computed by alu */
	int regDst;					/* destination register number, chosen between rt and rd */
  //......
};
  

/*do fetch stage*/
void do_if();

/*do decode stage*/
void do_id();

/*do execute stage*/
void do_ex();

/*do memory stage*/
void do_mem();

/*do write_back to register*/
void do_wb();


#define MD_FETCH_INSTI(INST, MEM, PC)					\
  { INST.a = MEM_READ_WORD(mem, (PC));					\
    INST.b = MEM_READ_WORD(mem, (PC) + sizeof(word_t)); }

#define SET_OPCODE(OP, INST) ((OP) = ((INST).a & 0xff)) 

#define RSI(INST)		(INST.b >> 24& 0xff)		/* reg source #1 */
#define RTI(INST)		((INST.b >> 16) & 0xff)		/* reg source #2 */
#define RDI(INST)		((INST.b >> 8) & 0xff)		/* reg dest */

#define IMMI(INST)	((int)((/* signed */short)(INST.b & 0xffff)))	/*get immediate value*/
#define TARGI(INST)	(INST.b & 0x3ffffff)		/*jump target*/

/**
 * macros for debug
 */
#define SP_DEBUG
#undef SP_DEBUG
#ifdef SP_DEBUG
#define PRINT_CYCLE(cycle) //fprintf(stderr, "Current Cycle: %d\n", cycle)
#define PRINT_REG(file, reg) fprintf(file, "r[%d]=%d\n", reg, GPR(reg))
#define PRINT_FD(file) fprintf(file, "IN IF/ID:\n\tPC: %x\n", fd.PC)
#define PRINT_DE(file) fprintf(file, "IN ID/EX:\n\tPC: %x data_1: %d data_2: %d imm: %d\n", \
		de.PC, de.reg_data_1, de.reg_data_2, de.imm)
#define PRINT_DE_SIG(file) fprintf(file, "IN ID/EX SIG: %d, %d, %d, %d, %d, %d, %d, %d\n", \
		de.signal.aluop, de.signal.alusrc, de.signal.regwrite, de.signal.regdst, de.signal.branch, de.signal.memwrite, de.signal.memread, de.signal.memtoreg)
#define PRINT_EM(file) fprintf(file, "IN EX/MEM:\n\tPC: %x TPC: %x ALUresult: %d zero: %d PCsrc: %d\n", \
		em.PC, em.TPC, em.ALUresult, em.zero, em.PCsrc)
#define PRINT_EM_SIG(file) fprintf(file, "IN EX/MEM SIG: %d, %d, %d, %d, %d, %d, %d, %d\n", \
		em.signal.aluop, em.signal.alusrc, em.signal.regwrite, em.signal.regdst, em.signal.branch, em.signal.memwrite, em.signal.memread, em.signal.memtoreg)
#define PRINT_MW(file) fprintf(file, "IN MEM/WB:\n\tPC: %x mem_data: %d alu_data: %d r[%d]=%d\n", \
		mw.PC, mw.mem_data, mw.alu_data, mw.regDst, GPR(mw.regDst))
#define PRINT_MW_SIG(file) fprintf(file, "IN MEM/WB SIG: %d, %d, %d, %d, %d, %d, %d, %d\n", \
		mw.signal.aluop, mw.signal.alusrc, mw.signal.regwrite, mw.signal.regdst, mw.signal.branch, mw.signal.memwrite, mw.signal.memread, mw.signal.memtoreg)
#define PRINT_PIPELINE(file) \
{\
	PRINT_FD(file);\
	PRINT_DE(file);\
	PRINT_DE_SIG(file);\
	PRINT_EM(file);\
	PRINT_EM_SIG(file);\
	PRINT_MW(file);\
	PRINT_MW_SIG(file);\
}
#define PRINT_INFO(file, MSG) fprintf(file, MSG)
#define PRINT_FORWARDING_SIG(file, sig_1, sig_2) fprintf(file, "IN do forwarding, set_sig_1: %d set_sig_2: %d\n", sig_1, sig_2)
#else
#define PRINT_CYCLE(cycle)
#define PRINT_REG(file, reg)
#define PRINT_FD(file)
#define PRINT_DE(file)
#define PRINT_DE_SIG(file)
#define PRINT_EM(file)
#define PRINT_EM_SIG(file)
#define PRINT_MW(file)
#define PRINT_MW_SIG(file)
#define PRINT_PIPELINE(file)
#define PRINT_INFO(file, MSG)
#define PRINT_FORWARDING_SIG(file, sig_1, sig_2)
#endif
