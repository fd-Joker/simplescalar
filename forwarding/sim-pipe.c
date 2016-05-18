#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* An implementation of 5-stage classic pipeline simulation */

/* don't count instructions flag, enabled by default, disable for inst count */
#undef NO_INSN_COUNT

#include "host.h"
#include "misc.h"
#include "machine.h"
#include "regs.h"
#include "memory.h"
#include "loader.h"
#include "syscall.h"
#include "dlite.h"
#include "sim.h"
#include "sim-pipe.h"

/* simulated registers */
static struct regs_t regs;

/* simulated memory */
static struct mem_t *mem = NULL;

/* register simulator-specific options */
void
sim_reg_options(struct opt_odb_t *odb)
{
  opt_reg_header(odb, 
"sim-pipe: This simulator implements based on sim-fast.\n"
		 );
}

/* check simulator-specific option values */
void
sim_check_options(struct opt_odb_t *odb, int argc, char **argv)
{
  if (dlite_active)
    fatal("sim-pipe does not support DLite debugging");
}

/* register simulator-specific statistics */
void
sim_reg_stats(struct stat_sdb_t *sdb)
{
#ifndef NO_INSN_COUNT
  stat_reg_counter(sdb, "sim_num_insn",
		   "total number of instructions executed",
		   &sim_num_insn, sim_num_insn, NULL);
#endif /* !NO_INSN_COUNT */
  stat_reg_int(sdb, "sim_elapsed_time",
	       "total simulation time in seconds",
	       &sim_elapsed_time, 0, NULL);
#ifndef NO_INSN_COUNT
  stat_reg_formula(sdb, "sim_inst_rate",
		   "simulation speed (in insts/sec)",
		   "sim_num_insn / sim_elapsed_time", NULL);
#endif /* !NO_INSN_COUNT */
  ld_reg_stats(sdb);
  mem_reg_stats(mem, sdb);
}


struct ifid_buf fd;
struct idex_buf de;
struct exmem_buf em;
struct memwb_buf mw;

#define DNA			(-1)

/* general register dependence decoders */
#define DGPR(N)			(N)
#define DGPR_D(N)		((N) &~1)

/* floating point register dependence decoders */
#define DFPR_L(N)		(((N)+32)&~1)
#define DFPR_F(N)		(((N)+32)&~1)
#define DFPR_D(N)		(((N)+32)&~1)

/* miscellaneous register dependence decoders */
#define DHI			(0+32+32)
#define DLO			(1+32+32)
#define DFCC		(2+32+32)
#define DTMP		(3+32+32)

/* initialize the simulator */
void
sim_init(void)
{
  /* allocate and initialize register file */
  regs_init(&regs);

  /* allocate and initialize memory space */
  mem = mem_create("mem");
  mem_init(mem);

  /* initialize stage latches*/
 
  /* IF/ID */
	fd.inst.a = NOP;
	fd.IFstop = 0;

  /* ID/EX */
	de.inst.a = NOP;
  de.latched = 0;
  /* EX/MEM */
	em.inst.a = NOP;

  /* MEM/WB */
	mw.inst.a = NOP;

}

/* load program into simulated state */
void
sim_load_prog(char *fname,		/* program to load */
	      int argc, char **argv,	/* program arguments */
	      char **envp)		/* program environment */
{
  /* load program text and data, set up environment, memory, and regs */
  ld_load_prog(fname, argc, argv, envp, &regs, mem, TRUE);
}

/* print simulator-specific configuration information */
void
sim_aux_config(FILE *stream)
{  
	/* nothing currently */
}

/* dump simulator-specific auxiliary simulator statistics */
void
sim_aux_stats(FILE *stream)
{  /* nada */}

/* un-initialize simulator-specific state */
void 
sim_uninit(void)
{ /* nada */ }


/*
 * configure the execution engine
 */

/* next program counter */
#define SET_NPC(EXPR)		(regs.regs_NPC = (EXPR))

/* current program counter */
#define CPC			(regs.regs_PC)

/* general purpose registers */
#define GPR(N)			(regs.regs_R[N])
#define SET_GPR(N,EXPR)		(regs.regs_R[N] = (EXPR))
#define DECLARE_FAULT(EXP) 	{;}
#if defined(TARGET_PISA)

/* floating point registers, L->word, F->single-prec, D->double-prec */
#define FPR_L(N)		(regs.regs_F.l[(N)])
#define SET_FPR_L(N,EXPR)	(regs.regs_F.l[(N)] = (EXPR))
#define FPR_F(N)		(regs.regs_F.f[(N)])
#define SET_FPR_F(N,EXPR)	(regs.regs_F.f[(N)] = (EXPR))
#define FPR_D(N)		(regs.regs_F.d[(N) >> 1])
#define SET_FPR_D(N,EXPR)	(regs.regs_F.d[(N) >> 1] = (EXPR))

/* miscellaneous register accessors */
#define SET_HI(EXPR)		(regs.regs_C.hi = (EXPR))
#define HI			(regs.regs_C.hi)
#define SET_LO(EXPR)		(regs.regs_C.lo = (EXPR))
#define LO			(regs.regs_C.lo)
#define FCC			(regs.regs_C.fcc)
#define SET_FCC(EXPR)		(regs.regs_C.fcc = (EXPR))

#endif

/* precise architected memory state accessor macros */
#define READ_BYTE(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_BYTE(mem, (SRC)))
#define READ_HALF(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_HALF(mem, (SRC)))
#define READ_WORD(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_WORD(mem, (SRC)))
#ifdef HOST_HAS_QWORD
#define READ_QWORD(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_QWORD(mem, (SRC)))
#endif /* HOST_HAS_QWORD */

#define WRITE_BYTE(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_BYTE(mem, (DST), (SRC)))
#define WRITE_HALF(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_HALF(mem, (DST), (SRC)))
#define WRITE_WORD(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_WORD(mem, (DST), (SRC)))
#ifdef HOST_HAS_QWORD
#define WRITE_QWORD(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_QWORD(mem, (DST), (SRC)))
#endif /* HOST_HAS_QWORD */

/* system call handler macro */
#define SYSCALL(INST)	sys_syscall(&regs, mem_access, mem, INST, TRUE)

#undef NO_INSN_COUNT
#ifndef NO_INSN_COUNT
#define INC_INSN_CTR()	sim_num_insn++
#else /* !NO_INSN_COUNT */
#define INC_INSN_CTR()	/* nada */
#endif /* NO_INSN_COUNT */

/* count the total number of cycle */
int count_cycle = 0;
/* print pipeline state */
void dump_pipeline(FILE *f) {
	enum md_fault_type _fault;
	fprintf(f, "[Cycle %d]--------------------------------------------------\n", count_cycle);
	md_inst_t inst;
 	MD_FETCH_INSTI(inst, mem, fd.PC + sizeof(md_inst_t))
	fprintf(f, "[IF] ");
	md_print_insn(inst, fd.PC + sizeof(md_inst_t), f);
	fprintf(f, "\n[ID] ");
	md_print_insn(fd.inst, fd.PC, f);
	fprintf(f, "\n[EX] ");
	md_print_insn(de.inst, de.PC, f);
	fprintf(f, "\n[MEM] ");
	md_print_insn(em.inst, em.PC, f);
	fprintf(f, "\n[WB] ");
	md_print_insn(mw.inst, mw.PC, f);
	fprintf(f, "\n[REGS] r[2]=%d r[3]=%d r[4]=%d r[5]=%d r[6]=%d mem=%d\n", 
			GPR(2), GPR(3), GPR(4), GPR(5), GPR(6), READ_WORD(16+GPR(30), _fault));
	// FIXME: debug
	PRINT_REG(f, 29);
	fprintf(f, "-------------------------------------------------------------\n");
}

#define REG_HAZARD(READ_PORT, WRITE_PORT) ((READ_PORT != 0) && (READ_PORT == WRITE_PORT))
/**
 * handle data hazard
 * @param set_oprand_1 and set_oprand_2: 0 - don`t set; 1 - use alu value; 2 - use WB value
 */
// FIXME: for SW_IMPL reg port and reg data are twisted, de.oprand.in1->de.reg_data_2 and de.oprand.in2->de.reg_data_1
void forwarding_unit(int *set_oprand_1, int *set_oprand_2, int em_regwrite, int mw_regwrite, 
		int de_in1, int de_in2, int em_regdst, int mw_regdst) {
	if (mw_regwrite && REG_HAZARD(de_in1, mw_regdst))
		*set_oprand_1 = 2;
	if (em_regwrite && REG_HAZARD(de_in1, em_regdst))
		*set_oprand_1 = 1;
	if (!(em_regwrite && REG_HAZARD(de_in1, em_regdst)) && !(mw_regwrite && REG_HAZARD(de_in1, mw_regdst)))
		*set_oprand_1 = 0;
	if (mw_regwrite && REG_HAZARD(de_in2, mw_regdst))
		*set_oprand_2 = 2;
	if (em_regwrite && REG_HAZARD(de_in2, em_regdst))
		*set_oprand_2 = 1;
	if (!(em_regwrite && REG_HAZARD(de_in2, em_regdst)) && !(mw_regwrite && REG_HAZARD(de_in2, mw_regdst)))
		*set_oprand_2 = 0;
}

/**
 * set control signal
 */
#define SET_CONSIG(ALUop, ALUsrc, RegWrite, RegDst, Branch, MemWrite, MemRead, MemToReg)\
	{\
		de.signal.aluop = ALUop;\
		de.signal.alusrc = ALUsrc;\
		de.signal.regwrite = RegWrite;\
		de.signal.regdst = RegDst;\
		de.signal.branch = Branch;\
		de.signal.memwrite = MemWrite;\
		de.signal.memread = MemRead;\
		de.signal.memtoreg = MemToReg;\
	}

/**
 * bubble the ID stage in case of branch miss prediction
 */
void do_bubble() {
	/* bubble the ID stage that has already passed */
	de.inst.a = NOP;
	de.inst.b = 0;
	de.opcode = NOP;
	SET_CONSIG(ALU_ADD, 0, 0, 0, B_NONE, 0, 0, 0);
}

/**
 * stall the IF and ID stage in case of data hazard
 */
void do_stall() {
	// FIXME: debug
	fprintf(stdout, "[DEBUG]In do stall de.in1: %d, de.in2: %d, em.regDst: %d mw.regwrite: %d if cond: %d\n", 
			de.oprand.in1, de.oprand.in2, em.regDst, mw.signal.regwrite, ((mw.signal.regwrite && (REG_HAZARD(de.oprand.in1, mw.regDst) || REG_HAZARD(de.oprand.in2, mw.regDst)))));
	if ((em.signal.regwrite && em.signal.memtoreg && (REG_HAZARD(de.oprand.in1, em.regDst) || REG_HAZARD(de.oprand.in2, em.regDst)))) {
		PRINT_INFO(stdout, "[DEBUG]IN do stall\n");
		fd.inst = de.inst;
		fd.PC = de.PC;
		de.inst.a = NOP;
		de.opcode = NOP;
		SET_CONSIG(ALU_ADD, 0, 0, 0, B_NONE, 0, 0, 0);
	}
}
void do_forward() {
	if (mw.signal.regwrite && REG_HAZARD(de.oprand.in1, mw.regDst))
		de.reg_data_1 = mw.signal.memtoreg ? mw.mem_data : mw.alu_data;
	if (mw.signal.regwrite && REG_HAZARD(de.oprand.in2, mw.regDst))
		de.reg_data_2 = mw.signal.memtoreg ? mw.mem_data : mw.alu_data;
	if (em.signal.regwrite && REG_HAZARD(de.oprand.in1, em.regDst))
		de.reg_data_1 = em.signal.memtoreg ? de.reg_data_1/* can not forward */ : em.ALUresult;
	if (em.signal.regwrite && REG_HAZARD(de.oprand.in2, em.regDst))
		de.reg_data_2 = em.signal.memtoreg ? de.reg_data_2/* can not forward */ : em.ALUresult;
}

/* start simulation, program loaded, processor precise state initialized */
void
sim_main(void)
{
  fprintf(stderr, "sim: ** starting *pipe* functional simulation **\n");

  /* must have natural byte/word ordering */
  if (sim_swap_bytes || sim_swap_words)
    fatal("sim: *pipe* functional simulation cannot swap bytes or words");

  /* set up initial default next PC */
  regs.regs_NPC = regs.regs_PC + sizeof(md_inst_t);
  /* maintain $r0 semantics */
  regs.regs_R[MD_REG_ZERO] = 0;
 
	/* test git */

	/* initialize fd.PC */
	fd.PC = regs.regs_PC - sizeof(md_inst_t);
  while (TRUE)
  {
		/* FIXME: debug */
		PRINT_PIPELINE(stdout);
		dump_pipeline(stdout);
		do_forward();
		do_stall();
		do_wb();
		do_mem();
		do_ex();
		do_id();
		do_if();

		PRINT_CYCLE(count_cycle);
		count_cycle++;

		if (MD_OPFIELD(mw.dumped_inst) == SYSCALL)
			SYSCALL(mw.dumped_inst);

		if (MD_OPFIELD(mw.dumped_inst) != NOP)
			INC_INSN_CTR();
  }
}

void do_if()
{
  md_inst_t instruction;
	if (fd.IFstop) {
		fd.inst.a = NOP;
	} else {
  	if (em.PCsrc==1){
			// FIXME: debug
			PRINT_INFO(stdout, "[debug]IN IF, branch taken\n");
  		fd.NPC = em.TPC;
			do_bubble();
  	}else{
  		fd.NPC = fd.PC + sizeof(md_inst_t);
  	}
  	fd.PC =fd.NPC;
//		fprintf(stderr, "[debug]fd.PC=%x\n", fd.PC);
  	MD_FETCH_INSTI(instruction, mem, fd.PC);
  	fd.inst = instruction;
		if (MD_OPFIELD(fd.inst) == SYSCALL)
			fd.IFstop = 1;
	}
}

void do_id()
{
	/**
	 * copy initial values of pipeline register
	 */
    de.inst = fd.inst;
    MD_SET_OPCODE(de.opcode, de.inst);
    de.PC = fd.PC;
    md_inst_t inst = de.inst;			/* used in machine.def */
		/**
		 * define the instruction and assign its in/out register index
		 */
#define DEFINST(OP,MSK,NAME,OPFORM,RES,FLAGS,O1,O2,I1,I2,I3)\
  if (OP==de.opcode){\
    de.instFlags = FLAGS;\
    de.oprand.out1 = O1;\
    de.oprand.out2 = O2;\
    de.oprand.in1 = I1;\
    de.oprand.in2 = I2;\
    de.oprand.in3 = I3;\
    goto READ_OPRAND_VALUE;\
  }
#define DEFLINK(OP,MSK,NAME,MASK,SHIFT)
#define CONNECT(OP)
#include "machine.def"
		/**
		 * decode the instruction
		 */
/* SET_CONSIG(ALUop, ALUsrc, RegWrite, RegDst, Branch, MemWrite, MemRead, MemToReg) */
#define SIGN_EXT(IMM_VALUE) (IMM_VALUE << 16)
READ_OPRAND_VALUE: 
	{
		// set instruction signal
		/* SET_CONSIG(ALUop, ALUsrc, RegWrite, RegDst, Branch, MemWrite, MemRead, MemToReg) */
		switch(de.opcode){
		case ADD:
			SET_CONSIG(ALU_ADD, 0, 1, 1, B_NONE, 0, 0, 0);break;
		case ADDU:
			SET_CONSIG(ALU_ADD, 0, 1, 1, B_NONE, 0, 0, 0);break;
		case ADDIU:
			SET_CONSIG(ALU_ADD, 1, 1, 0, B_NONE, 0, 0, 0);break;
		case SLTI:
			SET_CONSIG(ALU_SLT, 1, 1, 0, B_NONE, 0, 0, 0);break;
		case LUI:
			SET_CONSIG(ALU_ADD, 1, 1, 0, B_NONE, 0, 0, 0);break;
		case SW:
			SET_CONSIG(ALU_ADD, 1, 0, 0, B_NONE, 1, 0, 0);break;
		case LW:
			SET_CONSIG(ALU_ADD, 1, 1, 0, B_NONE, 0, 1, 1);break;
		case BNE:
			SET_CONSIG(ALU_SLT, 0, 0, 0, B_NE, 0, 0, 0);break;
		case JUMP:
			SET_CONSIG(ALU_SLT, 0, 0, 0, B_J, 0, 0, 0);break;
		case SLL:
			SET_CONSIG(ALU_SHL, 1, 1, 1, B_NONE, 0, 0, 0);break;
		default:
			PRINT_INFO(stdout, "IN ID, default branch taken\n");
			SET_CONSIG(ALU_ADD, 0, 0, 0, B_NONE, 0, 0, 0);
		}
		// read oprand value
		switch(de.opcode){
			case ADD:
				de.reg_data_1 = GPR(de.oprand.in1);
				de.reg_data_2 = GPR(de.oprand.in2);
				de.imm = 0;
				de.reg_dst_rt = 0;
				de.reg_dst_rd = de.oprand.out1;
				break;
			case ADDU:
				de.reg_data_1 = GPR(de.oprand.in1);
				de.reg_data_2 = GPR(de.oprand.in2);
				de.imm = 0;
				de.reg_dst_rt = 0;
				de.reg_dst_rd = de.oprand.out1;
				break;
			case ADDIU:
				de.reg_data_1 = GPR(de.oprand.in1);
				de.reg_data_2 = 0;
				de.imm = IMM;
				de.reg_dst_rt = de.oprand.out1;
				de.reg_dst_rd = 0;
				break;
			case SLTI:
				de.reg_data_1 = GPR(de.oprand.in1);
				de.reg_data_2 = 0;
				de.imm = IMM;
				de.reg_dst_rt = de.oprand.out1;
				de.reg_dst_rd = 0;
				break;
			case LUI:
				de.reg_data_1 = 0;
				de.reg_data_2 = 0;
				de.imm = UIMM << 16; // FIXME: LUI semantic is different from LUI_IMPL in machine.def
				de.reg_dst_rt = de.oprand.out1;
				de.reg_dst_rd = 0;
				break;
			case SW:
				// FIXME: reg_data_1 and reg_data_2 are twisted
				{
					int tmp = de.oprand.in1;
					de.oprand.in1 = de.oprand.in2;
					de.oprand.in2 = tmp;
				}
				de.reg_data_1 = GPR(de.oprand.in1); // GPR(BS)
				de.reg_data_2 = GPR(de.oprand.in2); // GPR(RT)
				de.imm = UIMM;
				de.reg_dst_rt = 0;
				de.reg_dst_rd = 0;
				break;
			case LW:
				de.reg_data_1 = GPR(de.oprand.in2); // GPR(BS)
				de.reg_data_2 = 0; // GPR(RT)
				de.imm = UIMM;
				de.reg_dst_rt = de.oprand.out1;
				de.reg_dst_rd = 0;
				break;
			case BNE:
				de.reg_data_1 = GPR(de.oprand.in1);
				de.reg_data_2 = GPR(de.oprand.in2);
				de.imm = UIMM;
				de.reg_dst_rt = 0;
				de.reg_dst_rd = 0;
				break;
			case JUMP:
				de.reg_data_1 = 0;
				de.reg_data_2 = 0;
				de.imm = TARG;
				de.reg_dst_rt = 0;
				de.reg_dst_rd = 0;
				break;
			case SLL:
				de.reg_data_1 = GPR(de.oprand.in1);
				de.reg_data_2 = 0;
				de.imm = UIMM;
				de.reg_dst_rt = 0;
				de.reg_dst_rd = de.oprand.out1;
				break;
			default:
				de.reg_data_1 = 0;
				de.reg_data_2 = 0;
				de.imm = 0;
				de.reg_dst_rt = 0;
				de.reg_dst_rd = 0;
		}
	}
}

#define COPY_SIGNAL(SIG_DST, SIG_SRC)\
{\
	SIG_DST.aluop = SIG_SRC.aluop;\
	SIG_DST.alusrc = SIG_SRC.alusrc;\
	SIG_DST.regwrite = SIG_SRC.regwrite;\
	SIG_DST.regdst = SIG_SRC.regdst;\
	SIG_DST.branch = SIG_SRC.branch;\
	SIG_DST.memwrite = SIG_SRC.memwrite;\
	SIG_DST.memread = SIG_SRC.memread;\
	SIG_DST.memtoreg = SIG_SRC.memtoreg;\
}
void do_ex()
{
	/**
	 * copy initial values of pipeline register
	 */
	em.inst = de.inst;
	em.PC = de.PC;
	em.opcode = de.opcode;
	em.reg_to_mem = de.reg_data_2;
//  md_inst_t inst = em.inst;
	/**
	 * copy signal
	 */
	COPY_SIGNAL(em.signal, de.signal);
	/**
	 * read oprand
	 */
	word_t oprand_1 = de.reg_data_1;
	word_t oprand_2 = de.signal.alusrc ? de.imm : de.reg_data_2;
	/**
	 * main alu
	 */
	switch(de.signal.aluop){
		case ALU_ADD:
			em.ALUresult = oprand_1 + oprand_2;
			em.zero = 0;
			break;
		case ALU_SUB:
			em.ALUresult = oprand_1 - oprand_2;
			em.zero = 0;
			break;
		case ALU_AND:
			em.ALUresult = oprand_1 & oprand_2;
			em.zero = 0;
			break;
		case ALU_OR:
			em.ALUresult = oprand_1 | oprand_2;
			em.zero = 0;
			break;
		case ALU_SLT:
			// FIXME: debug
			fprintf(stdout, "[debug]IN ALU, SLT oprand_1: %d oprand_2: %d\n", oprand_1, oprand_2);
			em.ALUresult = oprand_1 - oprand_2;
			em.zero = em.ALUresult ? 0 : 1;
			em.ALUresult = em.ALUresult >> 31;			/* semantic of set on less than */
			break;
		case ALU_SHL:
			em.ALUresult = oprand_1 << oprand_2;
			em.zero = 0;
			break;
	}
	/**
	 * compute TPC
	 */
#define SHL2(IMM_VALUE) (IMM_VALUE << 2)
	switch(de.signal.branch){
		case B_J:
			em.TPC = (de.PC & 0xF0000000) + SHL2(de.imm);
			em.PCsrc = 1;
			break;
		case B_NE:
			PRINT_INFO(stdout, "[DEBUG]IN EX, BNE\n")
			em.TPC = de.PC + sizeof(md_inst_t) + SHL2(de.imm);
			em.PCsrc = !(em.zero);
			break;
		default:
			em.PCsrc = 0;
	}
	/**
	 * select destination register
	 */
	em.regDst = de.signal.regdst ? de.reg_dst_rd : de.reg_dst_rt;
}

void do_mem()
{
	/**
	 * copy initial values of pipeline register
	 */
  mw.inst = em.inst;
	mw.PC = em.PC;
	mw.regDst = em.regDst;
	/**
	 * copy signal
	 */
	COPY_SIGNAL(mw.signal, em.signal);
	/**
	 * copy alu result
	 */
	mw.alu_data = em.ALUresult;
  /**
	 * access memory
	 */
	enum md_fault_type _fault;
	md_addr_t addr = (md_addr_t)em.ALUresult;
	word_t data;
	if (em.signal.memwrite) {
		PRINT_INFO(stdout, "[debug]IN MEM, write\n");
	  data = em.reg_to_mem;
		WRITE_WORD(data, addr, _fault);
	}
	if (em.signal.memread) {
		PRINT_INFO(stdout, "[debug]IN MEM, read\n");
		data = READ_WORD(addr, _fault);
		mw.mem_data = data;
	}
}

void do_wb()
{
	/**
	 * copy the instruction and pc value that comes out of the pipeline
	 */
  mw.dumped_inst = mw.inst;
	mw.dumped_PC = mw.PC;
	word_t data = mw.signal.memtoreg ? mw.mem_data : mw.alu_data;
	if (mw.signal.regwrite) {
		SET_GPR(mw.regDst, data);
	}
}

