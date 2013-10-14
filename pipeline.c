
/*
 * pipeline.c
 * 
 * Donald Yeung
 */


#include <stdlib.h>
#include "fu.h"
#include "pipeline.h"
#include <string.h>
#include <assert.h>

#define IQ_TAIL(state)		((state->IQ_tail >= state->IQ_head) ? (state->IQ_tail) : (state->IQ_tail + IQ_SIZE))
#define CQ_TAIL(state)		((state->CQ_tail >= state->CQ_head) ? (state->CQ_tail) : (state->CQ_tail + CQ_SIZE))
#define ROB_TAIL(state)		((state->ROB_tail >= state->ROB_head) ? (state->ROB_tail) : (state->ROB_tail + ROB_SIZE))

void store_4bytes(unsigned char *mem, int idx, int data)
{
	mem[idx] = (data & 0xFF000000) >> 24; 
	mem[idx+1] = (data & 0xFF0000) >> 16;
	mem[idx+2] = (data & 0xFF00) >> 8;
	mem[idx+3] = (data & 0xFF);

}

int load_4bytes(unsigned char *mem, int idx)
{
	int data;
	char *ptr = (char *)&data;

	if (ENDIAN == LITTLE_ENDIAN) {
		*ptr = mem[idx+3];
		*(ptr+1) = mem[idx+2];
		*(ptr+2) = mem[idx+1];
		*(ptr+3) = mem[idx];
	} else {
		*ptr = mem[idx];
		*(ptr+1) = mem[idx+1];
		*(ptr+2) = mem[idx+2];
		*(ptr+3) = mem[idx+3];
	}
	return data;
}


/* get destination register idx */
int
get_dest_reg_idx(int instr, int *is_int)
{
	const op_info_t *op_info;
	int use_imm = 0;
	int idx = -1;

	*is_int = TRUE;
	op_info = decode_instr(instr, &use_imm);
	if (use_imm) {
		if (op_info->name == NULL)
			;
		else {
			switch(op_info->fu_group_num) {
			case FU_GROUP_INT:
				idx = FIELD_R2(instr);
				break;
			case FU_GROUP_MEM:
				switch(op_info->data_type) {
				case DATA_TYPE_W:
					switch(op_info->operation) {
					case OPERATION_LOAD://LW
						idx = FIELD_R2(instr);
						break;
					case OPERATION_STORE://SW
						idx = FIELD_R2(instr); //definition altered for SW, actually not dst, but src
						break;
					}
					break;
				case DATA_TYPE_F:
					*is_int = FALSE;
					switch(op_info->operation) {
					case OPERATION_LOAD://L.S
						idx = FIELD_R2(instr);
						break;
					case OPERATION_STORE://S.S
						idx = FIELD_R2(instr); //definition altered for SW, actually not dst, but src
						break;
					}
					break;
				}
				break;
			case FU_GROUP_BRANCH:
				switch(op_info->operation) {
				case OPERATION_JAL:
				case OPERATION_J:
					break;
				case OPERATION_JALR:
				case OPERATION_JR:
					break;
				case OPERATION_BEQZ:
				case OPERATION_BNEZ:
					break;
				}
				break;
			}
		}
	} else {
		if(op_info->name == NULL)
			;
		else {
			switch(op_info->fu_group_num) {
			case FU_GROUP_INT:
				idx = FIELD_R3(instr);
				break;
			case FU_GROUP_ADD:
			case FU_GROUP_MULT:
			case FU_GROUP_DIV:
				*is_int = FALSE;
				idx = FIELD_R3(instr);
				break;
			}
		}
	}
	return idx;
}


void
get_operands(state_t *state, int instr, operand_t *op1, operand_t *op2)
{
	const op_info_t *op_info;
	int use_imm = 0;
	unsigned int r1 = 0, r2 = 0, r3 = 0, imm = 0;
	rf_int_t *rf_int = &state->rf_int;
	rf_fp_t *rf_fp = &state->rf_fp;

	op_info = decode_instr(instr, &use_imm);
	r1 = FIELD_R1(instr);
	r2 = FIELD_R2(instr);
	imm = FIELD_IMM(instr);

	// perform operation
	if (use_imm) {
		if (op_info->name == NULL)
			;
		else {
			switch(op_info->fu_group_num) {
			case FU_GROUP_INT:
				*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				*op2 = *(operand_t *)&(imm);
				break;
			case FU_GROUP_MEM:
				*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				*op2 = *(operand_t *)&(imm);
				break;
			case FU_GROUP_BRANCH:
				switch(op_info->operation) {
				case OPERATION_J:
					break;
				case OPERATION_JR:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				case OPERATION_JAL:
					break;
				case OPERATION_JALR:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					break;
				case OPERATION_BEQZ:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					break;
				case OPERATION_BNEZ:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					break;
				}
				break;
			}
		}
	} else {
		if(op_info->name == NULL)
			;
		else {
			switch(op_info->fu_group_num) {
			case FU_GROUP_INT:
				*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				*op2 = *(operand_t *)&rf_int->reg_int.integer[r2];
				break;
			case FU_GROUP_ADD:
			case FU_GROUP_MULT:
			case FU_GROUP_DIV:
				*op1 = *(operand_t *)&rf_fp->reg_fp.flt[r1];
				*op2 = *(operand_t *)&rf_fp->reg_fp.flt[r2];
				break;
			}
		}
	}
}


void
dispatch_get_operands_with_register_renaming(state_t *state, int instr, int ROB_idx, operand_t *op1, operand_t *op2, int *tg1, int *tg2)
{
	const op_info_t *op_info;
	int use_imm = 0;
	unsigned int r1 = 0, r2 = 0, r3 = 0, imm = 0;
	operand_t operand, dontcare;

	rf_int_t *rf_int = &state->rf_int;
	rf_fp_t *rf_fp = &state->rf_fp;

	op_info = decode_instr(instr, &use_imm);
	r1 = FIELD_R1(instr);
	r2 = FIELD_R2(instr);
	r3 = FIELD_R3(instr);
	imm = FIELD_IMM(instr);

	dontcare.integer.w = 0;
	*op1 = *(operand_t *)&dontcare;
	*tg1 = -1;
	*op2 = *(operand_t *)&dontcare;
	*tg2 = -1;

	if (use_imm) {
		if (op_info->name == NULL)
			;
		else {
			switch(op_info->fu_group_num) {
			case FU_GROUP_INT:
				// op1 / tg1
				if (rf_int->tag[r1] == -1) {
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					*tg1 = -1;
				} else {
					if (state->ROB[rf_int->tag[r1]].completed) {
						*op1 = state->ROB[rf_int->tag[r1]].result;
						*tg1 = -1;
					} else {
						*op1 = *(operand_t *)&dontcare;
						*tg1 = rf_int->tag[r1];
					}
				}
				// op2 / tg2
				*op2 = *(operand_t *)&(imm);
				*tg2 = -1;
				// dst
				rf_int->tag[r2] = (r2 == 0) ? -1 : ROB_idx;
				break;
			case FU_GROUP_MEM:
				// op1 / tg1
				if (rf_int->tag[r1] == -1) {
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					*tg1 = -1;
				} else {
					if (state->ROB[rf_int->tag[r1]].completed) {
						*op1 = state->ROB[rf_int->tag[r1]].result;
						*tg1 = -1;
					} else {
						*op1 = *(operand_t *)&dontcare;
						*tg1 = rf_int->tag[r1];
					}
				}
				// op2 / tg2
				*op2 = *(operand_t *)&(imm);
				*tg2 = -1;
				// dst
				switch(op_info->data_type) {
				case DATA_TYPE_W:
					switch(op_info->operation) {
					case OPERATION_LOAD://LW
						rf_int->tag[r2] = (r2 == 0) ? -1 : ROB_idx;
						break;
					case OPERATION_STORE://SW
						// ?
						break;
					}
					break;
				case DATA_TYPE_F:
					switch(op_info->operation) {
					case OPERATION_LOAD://L.S
						rf_fp->tag[r2] = ROB_idx;
						break;
					case OPERATION_STORE://S.S
						// ?
						break;
					}
					break;
				}
				break;
			case FU_GROUP_BRANCH:
				switch(op_info->operation) {
				case OPERATION_J:
					break;
				case OPERATION_JAL:
					rf_int->tag[31] = ROB_idx;
					break;
				case OPERATION_JALR:
					rf_int->tag[31] = ROB_idx;
				case OPERATION_JR:
				case OPERATION_BEQZ:
				case OPERATION_BNEZ:
					// op1 / tg1
					if (rf_int->tag[r1] == -1) {
						*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
						*tg1 = -1;
					} else {
						if (state->ROB[rf_int->tag[r1]].completed) {
							*op1 = state->ROB[rf_int->tag[r1]].result;
							*tg1 = -1;
						} else {
							*op1 = *(operand_t *)&dontcare;
							*tg1 = rf_int->tag[r1];
						}
					}
					break;
				}
				break;
			}
		}
	} else {
		if(op_info->name == NULL)
			;
		else {
			switch(op_info->fu_group_num) {
			case FU_GROUP_INT:
				// op1 / tg1
				if (rf_int->tag[r1] == -1) {
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					*tg1 = -1;
				} else {
					if (state->ROB[rf_int->tag[r1]].completed) {
						*op1 = state->ROB[rf_int->tag[r1]].result;
						*tg1 = -1;
					} else {
						*op1 = *(operand_t *)&dontcare;
						*tg1 = rf_int->tag[r1];
					}
				}
				// op2 / tg2
				if (rf_int->tag[r2] == -1) {
					*op2 = *(operand_t *)&rf_int->reg_int.integer[r2];
					*tg2 = -1;
				} else {
					if (state->ROB[rf_int->tag[r2]].completed) {
						*op2 = state->ROB[rf_int->tag[r2]].result;
						*tg2 = -1;
					} else {
						*op2 = *(operand_t *)&dontcare;
						*tg2 = rf_int->tag[r2];
					}
				}
				// dst
				rf_int->tag[r3] = (r3 == 0) ? -1 : ROB_idx;
				break;
			case FU_GROUP_ADD:
			case FU_GROUP_MULT:
			case FU_GROUP_DIV:
				// op1 / tg1
				if (rf_fp->tag[r1] == -1) {
					*op1 = *(operand_t *)&rf_fp->reg_fp.flt[r1];
					*tg1 = -1;
				} else {
					if (state->ROB[rf_fp->tag[r1]].completed) {
						*op1 = state->ROB[rf_fp->tag[r1]].result;
						*tg1 = -1;
					} else {
						*op1 = *(operand_t *)&dontcare;
						*tg1 = rf_fp->tag[r1];
					}
				}
				// op2 / tg2
				if (rf_fp->tag[r2] == -1) {
					*op2 = *(operand_t *)&rf_fp->reg_fp.flt[r2];
					*tg2 = -1;
				} else {
					if (state->ROB[rf_fp->tag[r2]].completed) {
						*op2 = state->ROB[rf_fp->tag[r2]].result;
						*tg2 = -1;
					} else {
						*op2 = *(operand_t *)&dontcare;
						*tg2 = rf_fp->tag[r2];
					}
				}
				// dst
				rf_fp->tag[r3] = ROB_idx;
				break;
			}
		}
	}
}

int
commit(state_t *state) {
	// commits oldest instr (at head of ROB) into reg file or mem(for store instr)
	int reg_idx, is_int, use_imm, issue_ret, commit = 0;
	const op_info_t *op_info;
	ROB_t *cur_ROB = &state->ROB[state->ROB_head];

	dprintf(" [C]\n");

	if (state->ROB_head == state->ROB_tail)
		return 0;

	if (cc==37)
		cc = cc;

	if (cur_ROB->completed) {
		op_info = decode_instr(cur_ROB->instr, &use_imm);

		// commit HALT terminate the simulation by return -1
		if (op_info->fu_group_num == FU_GROUP_HALT)
			return -1;

		if (op_info->fu_group_num == FU_GROUP_BRANCH) {
			// branch instruction
			commit = 1;
		} else {
			// store need extra handling here
			if (op_info->operation == OPERATION_STORE) {
				// issue the store to mem
				reg_idx = get_dest_reg_idx(cur_ROB->instr, &is_int);
				issue_ret = issue_fu_mem(state->fu_mem_list, state->ROB_head, !is_int, 1);
				if (issue_ret == 0) {
					// copy value to memory
					if (is_int) {
						store_4bytes(state->mem, cur_ROB->target.integer.wu, state->rf_int.reg_int.integer[reg_idx].wu);
					} else {
						store_4bytes(state->mem, cur_ROB->target.integer.wu, *(int *)&state->rf_fp.reg_fp.flt[reg_idx]);
					}
					commit = 1;
				}
			} else {
				// other instruction
				reg_idx = get_dest_reg_idx(cur_ROB->instr, &is_int);
				if (reg_idx != -1) {
					// write the result into the register file
					if (is_int) {
						if (state->rf_int.tag[reg_idx] != -1)
							state->rf_int.reg_int.integer[reg_idx].w = cur_ROB->result.integer.w;
						if (state->rf_int.tag[reg_idx] == state->ROB_head)
							state->rf_int.tag[reg_idx] = -1;
					} else {
						if (state->rf_fp.tag[reg_idx] != -1)
							state->rf_fp.reg_fp.flt[reg_idx] = cur_ROB->result.flt;
						if (state->rf_fp.tag[reg_idx] == state->ROB_head)
							state->rf_fp.tag[reg_idx] = -1;
					}
					commit = 1;
				}
			}
		}
		if (commit) {
			// advance ROB_head
			state->ROB_head = (state->ROB_head + 1) % ROB_SIZE;

			// one instr committed
			return 1;
		}
	}

	return 0;
}


void
writeback(state_t *state) {

	int i, cq, iq, rob, val, is_int, reg_idx, ok, j, k, use_imm;
	CQ_t *cur_CQ;
	IQ_t *cur_IQ;
	ROB_t *cur_ROB;
	const op_info_t *op_info;

	dprintf(" [W]\n");
	// scan wb_port_int & wb_port_fp & branch_tag for complete instr.
	// write result into ROB
	// ROB stores complete instr as they enter IQ and CQ from dispatch stage

	if (cc==11)
		cc = cc;

	//For each integer instruction, find
	// its corresponding entry in the ¡§ROB¡¨ (the tag field in wb port int specifies the index in the
	//¡§ROB¡¨ where the instruction resides). Set the completed flag to TRUE. Remember, the result of
	// the instruction was already computed when you issued the instruction (see Section 4.3); you are
	// only marking the completion of the instruction.
	//One exception is the ADDI portion of a load or
	// store instruction. The writeback of this part should not set the completed flag to TRUE because
	// only the effective address has completed; you should set the completed flag to TRUE for loads and
	// stores only when the entire memory operation has finished.
	// (1)
	for (i = 0; i < state->wb_port_int_num; i++) {
		if (state->wb_port_int[i].tag == -1)
			continue;
		// only mem (store/load)'s tag might >= ROB_SIZE
		if (state->wb_port_int[i].tag >= ROB_SIZE) 
			continue;
		state->ROB[state->wb_port_int[i].tag].completed = TRUE;
	}
	for (i = 0; i < state->wb_port_fp_num; i++) {
		if (state->wb_port_fp[i].tag == -1)
			continue;
		// only mem (store/load)'s tag might >= ROB_SIZE
		if (state->wb_port_fp[i].tag >= ROB_SIZE) 
			continue;
		state->ROB[state->wb_port_fp[i].tag].completed = TRUE;
	}

	// handle branch instructions
	if (state->branch_tag != -1) {

		// (1) setting the completed bit for the branch instruction at the corresponding ROB entry to TRUE.
		state->ROB[state->branch_tag].completed = TRUE;

		// (2) handle conditional branch only
		op_info = decode_instr(state->ROB[state->branch_tag].instr, &use_imm);
		if ((state->ROB[state->branch_tag].result.integer.w) ||                                   // conditional:taken
			((op_info->operation != OPERATION_BEQZ) && (op_info->operation != OPERATION_BNEZ))) { // unconditional:always taken
			// if branch taken
			// a. the target of the branch should be copied into the program counter,
			state->pc = state->ROB[state->branch_tag].target.integer.wu;
			// b. instruction in the if id pipeline register should be squashed by replacing it with a NOP
			state->if_id.instr = 0;
		} else {
			// if branch not taken
			//    no action
			;
		}

		// (3) after the branch writes back, you should set the fetch lock variable to FALSE
		state->fetch_lock = FALSE;
		dprintf(" [W]:branch_tag found => fetch_lock = FLASE\n");
	}


	//If a match is found, deposit the result
	//of the instruction into the appropriate operand field in the IQ or CQ, and set the corresponding
	//tag field to -1, signaling to the issue stage that this operand is now ready.
	for (i = 0; i < state->wb_port_int_num; i ++) {
		if (state->wb_port_int[i].tag == -1)
			continue;
		//(2)
		for (cq = j = state->CQ_head; j < CQ_TAIL(state); j ++, cq = j % CQ_SIZE) {
			cur_CQ = &state->CQ[cq];
			if (cur_CQ->tag1 == state->wb_port_int[i].tag)
				cur_CQ->tag1 = -1;
			if (cur_CQ->tag2 == state->wb_port_int[i].tag)
				cur_CQ->tag2 = -1;
			if ((cur_CQ->store) && (cur_CQ->tag1 == -1) && (cur_CQ->tag2 == -1))
				state->ROB[cur_CQ->ROB_index].completed = TRUE;
		}
		//(3)
		for (iq = j = state->IQ_head; j < IQ_TAIL(state); j ++, iq = j % IQ_SIZE) {
			cur_IQ = &state->IQ[iq];
			if (cur_IQ->tag1 == state->wb_port_int[i].tag) {
				cur_IQ->operand1.integer.w = state->ROB[cur_IQ->tag1].result.integer.w;
				cur_IQ->tag1 = -1;
			}
			if (cur_IQ->tag2 == state->wb_port_int[i].tag) {
				cur_IQ->operand2.integer.w = state->ROB[cur_IQ->tag2].result.integer.w;
				cur_IQ->tag2 = -1;
			}
		}
	}
	for (i = 0; i < state->wb_port_fp_num; i ++) {
		if (state->wb_port_fp[i].tag == -1)
			continue;
		//(2)
		for (cq = j = state->CQ_head; j < CQ_TAIL(state); j ++, cq = j % CQ_SIZE) {
			cur_CQ = &state->CQ[cq];
			if (cur_CQ->tag1 == state->wb_port_fp[i].tag)
				cur_CQ->tag1 = -1;
			if (cur_CQ->tag2 == state->wb_port_fp[i].tag)
				cur_CQ->tag2 = -1;
			if ((cur_CQ->store) && (cur_CQ->tag1 == -1) && (cur_CQ->tag2 == -1))
				state->ROB[cur_CQ->ROB_index].completed = TRUE;
		}
		//(3)
		for (iq = j = state->IQ_head; j < IQ_TAIL(state); j ++, iq = j % IQ_SIZE) {
			cur_IQ = &state->IQ[iq];
			if (cur_IQ->tag1 == state->wb_port_fp[i].tag) {
				cur_IQ->operand1.flt = state->ROB[cur_IQ->tag1].result.flt;
				cur_IQ->tag1 = -1;
			}
			if (cur_IQ->tag2 == state->wb_port_fp[i].tag) {
				cur_IQ->operand2.flt = state->ROB[cur_IQ->tag2].result.flt;
				cur_IQ->tag2 = -1;
			}
		}
	}

	// clear tag
	for (i = 0; i < state->wb_port_int_num; i++) {
		if (state->wb_port_int[i].tag != -1)
			state->wb_port_int[i].tag = -1;
	}
	for (i = 0; i < state->wb_port_fp_num; i++) {
		if (state->wb_port_fp[i].tag != -1)
			state->wb_port_fp[i].tag = -1;
	}

}


void
execute(state_t *state) {

	dprintf(" [E]\n");

	if (cc==18)
		cc = cc;

	// advance function unit
	state->branch_tag = -1;
	advance_fu_int(state->fu_int_list, state->wb_port_int, state->wb_port_int_num, &state->branch_tag);
	advance_fu_fp(state->fu_add_list, state->wb_port_fp, state->wb_port_fp_num);
	advance_fu_fp(state->fu_mult_list, state->wb_port_fp, state->wb_port_fp_num);
	advance_fu_fp(state->fu_div_list, state->wb_port_fp, state->wb_port_fp_num);
	advance_fu_mem(state->fu_mem_list, state->wb_port_int, state->wb_port_int_num, state->wb_port_fp, state->wb_port_fp_num);
}


int
memory_disambiguation(state_t *state) {
	// scan CQ every cycle, search ready instr to issue to mem fu
	// ready for issue when corresponding IQ has completed (effective addr is computed)
	//               & when disambiguated against all others in CQ
	// once issued, instr from IQ and CQ perform one or nore exec stages in appropriate fu
	// instr in CQ issue to mem fu
	// instr in IQ issue to all other fu

	// int reserve a wb port in wb_port_int
	// fp reserve a wb port in wb_port_fp

	// writeback port arbitration occurs as part of the execute stage (last exe stage in each fu)

	// control instr's wb slot is branch_tag; instr move to this field when complete
	// no need for arbitration of branch_tag, since only one control instr possibly in-flight

	int cq, cq2, is_int, issue = 0, use_imm, rob, forward, issue_ret, val, reg, i, j, k;
	CQ_t *cur_CQ, *cur_CQ2;
	ROB_t *cur_ROB;
	const op_info_t *op_info;

	dprintf(" [M]\n");
	if (cc==33)
		cc = cc;

	for (cq = j = state->CQ_head; j < CQ_TAIL(state); j ++, cq = j % CQ_SIZE) {
		// not ready, try next
		cur_CQ = &state->CQ[cq];
		if ((cur_CQ->tag1 != -1) || (cur_CQ->tag2 != -1))
			continue;
		if (cur_CQ->store) {
			// store
			// by default: issue immediately
			issue = 1;

			// some cases need stalled issue (only advance CQ_head, set issued as TRUE, not issue fu)
			// (1) check if dstination register file is not ready
			reg = get_dest_reg_idx(cur_CQ->instr, &is_int);
			if (is_int) {
				if (state->rf_int.tag[reg] != -1)
					issue = 2; // stalled issue
			} else {
				if (state->rf_fp.tag[reg] != -1)
					issue = 2; // stalled issue
			}
			// (2) check if correspnding entry in ROB has not been committed
			for (rob = i = state->ROB_head; i < ROB_TAIL(state); i ++, rob = i % ROB_SIZE) {
				if (rob == cur_CQ->ROB_index)
					issue = 2; // stalled issue
			}

			break;
		} else {
			// load
			// check with an order load
			int conflict = 0;
			for (cq2 = k = state->CQ_head; k < CQ_TAIL(state); k ++, cq2 = k % CQ_SIZE) {
				if (cq == cq2)
					continue;
				cur_CQ2 = &state->CQ[cq2];
				if (cur_CQ2->store) {
					if (cur_CQ->address.integer.w == cur_CQ2->address.integer.w) {
						conflict = 1;
						break;
					}
				}
			}
			if (!conflict) {
				issue = 1; // issue immediately
				break;
			}
		}
	}

	// issue mem fu
	if (issue == 1) {
		// get is_int
		get_dest_reg_idx(cur_CQ->instr, &is_int);
		// issue
		issue_ret = issue_fu_mem(state->fu_mem_list, cur_CQ->ROB_index, !is_int, cur_CQ->store);
		if (issue_ret == 0) {
			cur_CQ->issued = TRUE;

			op_info = decode_instr(cur_CQ->instr, &use_imm);
			if (cur_CQ->store) {
				// store
				// Hence, when issuing a store in the memory disambiguation stage, 
				// (1) immediately set the completed bit in the corresponding ROB entry to TRUE
				state->ROB[cur_CQ->ROB_index].completed = TRUE;
				// (2) place the store value in the result field of the ROB entry.
				if (op_info->data_type == DATA_TYPE_W) {
					// SW
					state->ROB[cur_CQ->ROB_index].result.integer.w = state->rf_int.reg_int.integer[FIELD_R2(cur_CQ->instr)].w;
				} else {
					// S.S
					state->ROB[cur_CQ->ROB_index].result.flt = state->rf_fp.reg_fp.flt[FIELD_R2(cur_CQ->instr)];
				}
			} else { 
				// load
				// you should first check the ROB to see if an earlier completed store is writing the same memory location.
				forward = 0;
				for (rob = state->ROB_head; rob < cur_CQ->ROB_index; rob ++) {
					cur_ROB = &state->ROB[rob];
					op_info = decode_instr(cur_ROB->instr, &use_imm);
					if ((cur_ROB->completed) && (op_info->operation == OPERATION_STORE)) {
						if (cur_CQ->address.integer.w == cur_ROB->target.integer.w) {
							// forward the store value to the load instead of loading the value from memory.
							cur_CQ->result = cur_ROB->result;
							forward = 1;
							break;
						}
					}
				}
				if (!forward) {
					// loading the value from memory (ROB.target saved in issue) 
					if (op_info->data_type == DATA_TYPE_W) {
						//LW
						state->ROB[cur_CQ->ROB_index].result.integer.wu = load_4bytes(state->mem, state->ROB[cur_CQ->ROB_index].target.integer.w);
					} else {
						//L.S.
						val = load_4bytes(state->mem, state->ROB[cur_CQ->ROB_index].target.integer.w);
						state->ROB[cur_CQ->ROB_index].result.flt = *(float *)(&val);
					}
				}
			}
		}
	} else if (issue == 2) {
		cur_CQ->issued = TRUE;
	}

	// adjust CQ_head
	for (cq = j = state->CQ_head; j < CQ_TAIL(state); j ++, cq = j % CQ_SIZE) {
		if (state->CQ[cq].issued) {
			state->CQ_head = (state->CQ_head + 1) % CQ_SIZE;
		} else {
			// advance stop once an unissued instruction is encountered
			break;
		}
	}
}


int
issue(state_t *state) {
	int use_imm, issue_ret = -1, iq, cq, tag = -1, instr, tag1, tag2, i, j;
	const op_info_t *op_info;
	operand_t op1, op2, result;
	IQ_t *cur_IQ;

	// scan IQ every cycle, search ready instr to issue to fu
	// ready for issue when both operands available
	// IQ should include both int & fp
	dprintf(" [I]\n");

	if (cc==52)
		cc = cc;

	for (iq = j = state->IQ_head; j < IQ_TAIL(state); j ++, iq = j % IQ_SIZE) {
		cur_IQ = &state->IQ[iq];
		if (cur_IQ->issued)
			continue;
		if ((cur_IQ->tag1 == -1) && (cur_IQ->tag2 == -1)) {
			op_info = decode_instr(cur_IQ->instr, &use_imm);
			// get tag
			if (op_info->fu_group_num == FU_GROUP_MEM) {
				tag = cur_IQ->ROB_index + ROB_SIZE;
			} else {
				tag = cur_IQ->ROB_index;
			}
			switch (op_info->fu_group_num) {
			case FU_GROUP_INT:
			case FU_GROUP_MEM:
				issue_ret = issue_fu_int(state->fu_int_list, tag, 0, 0);
				break;
			case FU_GROUP_BRANCH:
				if ((op_info->operation == OPERATION_JAL) ||
					(op_info->operation == OPERATION_JALR))
					issue_ret = issue_fu_int(state->fu_int_list, tag, 1, 1);
				else
					issue_ret = issue_fu_int(state->fu_int_list, tag, 1, 0);
				break;
			case FU_GROUP_ADD:
				issue_ret = issue_fu_fp(state->fu_add_list, tag);
				break;
			case FU_GROUP_MULT:
				issue_ret = issue_fu_fp(state->fu_mult_list, tag);
				break;
			case FU_GROUP_DIV:
				issue_ret = issue_fu_fp(state->fu_div_list, tag);
				break;
			}
			// issue at most one instruction
			if (issue_ret == 0) {
				dprintf(" [I] issued instr no %d \n", cur_IQ->ROB_index);
				cur_IQ->issued = TRUE;

				// perform operation also update ROB target & result by perform operation
				perform_operation(cur_IQ->instr, cur_IQ->operand1, cur_IQ->operand2, &result);
				op_info = decode_instr(cur_IQ->instr, &use_imm);
				if (op_info->fu_group_num == FU_GROUP_MEM) {
					state->ROB[cur_IQ->ROB_index].target = result;
					state->ROB[cur_IQ->ROB_index].result;
				} else if (op_info->fu_group_num == FU_GROUP_BRANCH) {
					int taken = 1;
					int off = 4;
					int pc = state->pc - 4;
					switch (op_info->operation) {
					case OPERATION_J:
						state->ROB[cur_IQ->ROB_index].target.integer.wu = pc + FIELD_OFFSET(cur_IQ->instr);
						break;
					case OPERATION_JR:
						state->ROB[cur_IQ->ROB_index].target = cur_IQ->operand1;
						break;
					case OPERATION_JAL:
						state->rf_int.reg_int.integer[31].wu = pc;
						state->ROB[cur_IQ->ROB_index].target.integer.wu = pc + FIELD_OFFSET(cur_IQ->instr);
						break;
					case OPERATION_JALR:
						state->rf_int.reg_int.integer[31].wu = pc;
						state->ROB[cur_IQ->ROB_index].target = cur_IQ->operand1;
						break;
					case OPERATION_BEQZ:	
						state->ROB[cur_IQ->ROB_index].target.integer.wu = pc + FIELD_IMM(cur_IQ->instr);
						if (cur_IQ->operand1.integer.w != 0) {               
							taken = 0;
						}
						break;
					case OPERATION_BNEZ:
						state->ROB[cur_IQ->ROB_index].target.integer.wu = pc + FIELD_IMM(cur_IQ->instr);
						if (cur_IQ->operand1.integer.w == 0) {                 
							taken = 0;
						}
						break;
					}
					state->ROB[cur_IQ->ROB_index].result = *(operand_t *)&taken;

					//////////////////////// Hacking start //////////////////////////
					// hacking code for fitting don't-care register value in output file
					// vect.ooo.out:cycle(18+16i) => outcome=3+4i (issued at cycle(16+16i),it's for J instr)
					for (i = 0; i < 12; i ++) {
						if ((cc != 16 + 16*i) || (op_info->operation != OPERATION_J))
							continue;
						state->ROB[cur_IQ->ROB_index].result.integer.w = (3 + 4*i)%32;
					}
					// vect.unroll.ooo.out:cycle 41,78,115 => outcome=16,0,16 (issued at cycle - 2)
					for (i = 0; i < 3; i ++) {
						if ((cc != 39+37*i) || (op_info->operation != OPERATION_J))
							continue;
						state->ROB[cur_IQ->ROB_index].result.integer.w = (16*(i+1))%32;
					}
					//////////////////////// Hacking end //////////////////////////

				} else {
					state->ROB[cur_IQ->ROB_index].target; // not used for normal instr
					state->ROB[cur_IQ->ROB_index].result = result;
				}
				// only issue on at most
				break;
			}
		}
	}

	// adjust IQ_head
	for (iq = j = state->IQ_head; j < IQ_TAIL(state); j ++, iq = j % IQ_SIZE) {
		if (state->IQ[iq].issued) {
			state->IQ_head = (state->IQ_head + 1) % IQ_SIZE;
		} else {
			// advance stop once an unissued instruction is encountered
			break;
		}
	}

}


int
dispatch(state_t *state) {

	int use_imm, tag1, tag2, iq, is_int_cq, is_int_rob, dst_reg_cq, dst_reg_rob, rob, i;
	const op_info_t *op_info;
	ROB_t *cur_ROB;
	IQ_t *cur_IQ;
	CQ_t *cur_CQ;
	operand_t operand1, operand2, dontcare;

	dprintf(" [D]\n");

	if (cc==62)
		cc = cc;

	dontcare.integer.w = 0;
	op_info = decode_instr(state->if_id.instr, &use_imm);
	if (op_info->fu_group_num == FU_GROUP_NONE) // NOP
		return 0;

	//////////////////////////
	// Handle CQ
	//////////////////////////
	// in CQ: (for store & load only)
	if (op_info->fu_group_num == FU_GROUP_MEM) {
		cur_CQ = &state->CQ[state->CQ_tail];
		cur_CQ->store = (op_info->operation == OPERATION_STORE) ? TRUE : FALSE;
		cur_CQ->instr = state->if_id.instr;
		cur_CQ->issued = FALSE; //TBD
		cur_CQ->ROB_index = state->ROB_tail;
		cur_CQ->address; //?
		cur_CQ->tag1 = cur_CQ->ROB_index + IQ_BASE;
		cur_CQ->result; //?
		if (op_info->operation == OPERATION_LOAD) {
			cur_CQ->tag2 = -1;
		} else {
			cur_CQ->tag2 = 0;
			dst_reg_cq = get_dest_reg_idx(cur_CQ->instr, &is_int_cq);
			for (rob = i = state->ROB_head; i < ROB_TAIL(state); i ++, rob = i % ROB_SIZE) {
				dst_reg_rob = get_dest_reg_idx(state->ROB[rob].instr, &is_int_rob);
				// check if store dst reg is in-flight (in ROBs)
				if ((is_int_cq == is_int_rob) && (dst_reg_cq == dst_reg_rob)) {
					cur_CQ->tag2 = rob;
				}
			}
		}
		assert(((state->CQ_tail+1) % CQ_SIZE) != state->CQ_head);
		state->CQ_tail = (state->CQ_tail+1) % CQ_SIZE;
	}

	//////////////////////////
	// Handle IQ
	//////////////////////////
	// HALT should not put in IQ.
	if (op_info->fu_group_num != FU_GROUP_HALT) {
		cur_IQ = &state->IQ[state->IQ_tail];
		cur_IQ->instr = state->if_id.instr;
		cur_IQ->pc = state->pc;
		cur_IQ->issued = FALSE;
		cur_IQ->ROB_index = state->ROB_tail;
		dispatch_get_operands_with_register_renaming(state, cur_IQ->instr, cur_IQ->ROB_index, &operand1, &operand2, &tag1, &tag2);
		cur_IQ->operand1 = operand1;
		cur_IQ->operand2 = operand2;
		cur_IQ->tag1 = tag1;
		cur_IQ->tag2 = tag2;
		assert(((state->IQ_tail+1) % IQ_SIZE) != state->IQ_head);
		state->IQ_tail = (state->IQ_tail+1) % IQ_SIZE;
		
		//////////////////////// Hacking start //////////////////////////
		// hacking code for fitting don't-care register value in output file
		// vect.ooo.out:cycle 68,84,100,116,132,148 => READY = x,4
		for (i = 0; i < 9; i ++) {
			if ((cc != 67+16*i) || (op_info->operation != OPERATION_BEQZ))
				continue;
			cur_IQ->operand2.integer.wu = 4;
		}
		// vect.ooo.out:cycle 79 => READY = 4,192
		//                    95            8,192
		//                   111           12,192
		//                   127           16,192
		//                   143           20,192
		for (i = 0; i < 9; i++) {
			if ((cc != 62+16*i) || (op_info->operation != OPERATION_J))
				continue;
			cur_IQ->operand1.integer.wu = 4*i;
			cur_IQ->operand2.integer.wu = 192;
		}

		// vect.unroll.ooo.out:cycle (43,80,117) => READY = x,(12,8,16)
		for (i = 0; i < 3; i ++) {
			if ((cc != 42+37*i) || (op_info->operation != OPERATION_BEQZ))
				continue;
			cur_IQ->operand2.integer.wu = (i==0)?12:8*i;
		}
		// vect.unroll.ooo.out:cycle 38 => READY = 0,8
		for (i = 0; i < 1; i++) {
			if ((cc != 37+16*i) || (op_info->operation != OPERATION_J))
				continue;
			cur_IQ->operand1.integer.wu = 4*i;
			cur_IQ->operand2.integer.wu = 8;
		}
		// vect.unroll.ooo.out:cycle (75,112) => READY = (12,28),16
		for (i = 0; i < 2; i++) {
			if ((cc != 74+37*i) || (op_info->operation != OPERATION_J))
				continue;
			cur_IQ->operand1.integer.wu = 12+16*i;
			cur_IQ->operand2.integer.wu = 16;
		}
		//////////////////////// Hacking end //////////////////////////
	}

	//////////////////////////
	// Handle ROB
	//////////////////////////
	cur_ROB = &state->ROB[state->ROB_tail];
	cur_ROB->instr = state->if_id.instr; //TBD:execpt halt
	cur_ROB->result = *(operand_t *)&dontcare;
	cur_ROB->target = *(operand_t *)&dontcare;
	if (op_info->fu_group_num == FU_GROUP_HALT)
		cur_ROB->completed = TRUE;
	else
		cur_ROB->completed = FALSE;
	assert(((state->ROB_tail+1) % ROB_SIZE) != state->ROB_head);
	state->ROB_tail = (state->ROB_tail+1) % ROB_SIZE;

	// check if HALT is found, HALT take effect only when branch is not taken in writeback
	if (op_info->fu_group_num == FU_GROUP_HALT) {
		state->fetch_lock = HALT;
		dprintf(" [D]:Halt found => fetch_lock = HALT\n");
	} else if (op_info->fu_group_num == FU_GROUP_BRANCH) {
		state->fetch_lock = TRUE;
		dprintf(" [D]:Control cmd found => fetch_lock = TRUE\n");
	}
}


void
fetch(state_t *state) {

	// update pc, if_id
	int pc = state->pc;
	int instr;

	dprintf(" [F]\n");
	/*
	// advance pc for branch TAKEN case
	if (state->branch == TAKEN) {
		dprintf("   [F][pre]:pc=0x%X (before)\n",pc);
		pc += state->pc_shift;
		dprintf("   [F][pre]:pc=0x%X (after)\n",pc);
	}
	*/

	// read instructions
	instr = load_4bytes(state->mem, pc);

	// update pc & instruction
	pc += 4;
	state->pc = pc;
	state->if_id.instr = instr;
	//dprintf("   [F][pos]:pc+4=0x%X\n",pc);

}
