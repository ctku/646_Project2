
/*
 * pipeline.c
 * 
 * Donald Yeung
 */


#include <stdlib.h>
#include "fu.h"
#include "pipeline.h"
#include <string.h>


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
					//printf("%s #%d",op_info->name,FIELD_OFFSET(instr));
					break;
				case OPERATION_JALR:
				case OPERATION_JR:
					printf("%s R%d",op_info->name,FIELD_R1(instr));
					break;
				case OPERATION_BEQZ:
				case OPERATION_BNEZ:
					;//printf("%s R%d #%d",op_info->name,FIELD_R1(instr),FIELD_IMM(instr));
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

int
get_idx_by_match_ROB_with_dest_reg(state_t *state, int src_is_int, int src_reg_idx, operand_t *result)
{
	int dst_is_int, dst_reg_idx, i;

	(*result).integer.w = 0;
	for (i=state->ROB_head; i<state->ROB_tail; i++) {
		dst_reg_idx = get_dest_reg_idx(state->ROB[i].instr, &dst_is_int);
		if ((src_is_int == dst_is_int) && (src_reg_idx == dst_reg_idx)) {
			if (state->ROB[i].completed)
				*result = state->ROB[i].result;
			return i;
		}
	}

	return -1;
}

void
get_operands(state_t *state, int instr, operand_t *op1, operand_t *op2, int *tag1, int *tag2, int reg_renaming)
{
	const op_info_t *op_info;
	int use_imm = 0;
	unsigned int r1 = 0, r2 = 0, r3 = 0, imm = 0;
	operand_t operand;

	rf_int_t *rf_int = &state->rf_int;
	rf_fp_t *rf_fp = &state->rf_fp;

	op_info = decode_instr(instr, &use_imm);
	r1 = FIELD_R1(instr);
	r2 = FIELD_R2(instr);
	r2 = FIELD_R2(instr);
	imm = FIELD_IMM(instr);

	*tag1 = -1;
	*tag2 = -1;

	if (use_imm) {
		if (op_info->name == NULL)
			;
		else {
			switch(op_info->fu_group_num) {
			case FU_GROUP_INT:
				*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				*op2 = *(operand_t *)&(imm);
				if (reg_renaming && (r1 != 0)) {
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1, &operand);
					rf_int->tag[r1] = *tag1;
					if (*tag1 != -1)
						*op1 = operand;
				}
				break;
			case FU_GROUP_MEM:
				*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				*op2 = *(operand_t *)&(imm);
				if (reg_renaming && (r1 != 0)) {
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1, &operand);
					rf_int->tag[r1] = *tag1;
					if (*tag1 != -1)
						*op1 = operand;
				}
				break;
			case FU_GROUP_BRANCH:
				switch(op_info->operation) {
				case OPERATION_J:
					break;
				case OPERATION_JR:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1, &operand);
						rf_int->tag[r1] = *tag1;
						if (*tag1 != -1)
							*op1 = operand;
					}
				case OPERATION_JAL:
					break;
				case OPERATION_JALR:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1, &operand);
						rf_int->tag[r1] = *tag1;
						if (*tag1 != -1)
							*op1 = operand;
					}
					break;
				case OPERATION_BEQZ:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1, &operand);
						rf_int->tag[r1] = *tag1;
						if (*tag1 != -1)
							*op1 = operand;
					}
					break;
				case OPERATION_BNEZ:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1, &operand);
						rf_int->tag[r1] = *tag1;
						if (*tag1 != -1)
							*op1 = operand;
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
				*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				*op2 = *(operand_t *)&rf_int->reg_int.integer[r2];
				if (reg_renaming && (r1 != 0)) {
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1, &operand);
					rf_int->tag[r1] = *tag1;
					if (*tag1 != -1)
						*op1 = operand;
				}
				if (reg_renaming && (r2 != 0)) {
					*tag2 = get_idx_by_match_ROB_with_dest_reg(state, 1, r2, &operand);
					rf_int->tag[r2] = *tag2;
					if (*tag2 != -1)
						*op2 = operand;
				}
				break;
			case FU_GROUP_ADD:
			case FU_GROUP_MULT:
			case FU_GROUP_DIV:
				*op1 = *(operand_t *)&rf_fp->reg_fp.flt[r1];
				*op2 = *(operand_t *)&rf_fp->reg_fp.flt[r2];
				if (reg_renaming) {
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 0, r1, &operand);
					rf_fp->tag[r1] = *tag1;
					if (*tag1 != -1)
						*op1 = operand;
					*tag2 = get_idx_by_match_ROB_with_dest_reg(state, 0, r2, &operand);
					rf_fp->tag[r2] = *tag2;
					if (*tag2 != -1)
						*op2 = operand;
				}
				break;
			}
		}
	}
}

void
get_operands_with_register_renaming(state_t *state, int instr, operand_t *op1, operand_t *op2, int *tag1, int *tag2)
{
	get_operands(state, instr, op1, op2, tag1, tag2, 1);
}

void
get_operands_without_register_renaming(state_t *state, int instr, operand_t *op1, operand_t *op2, int *tag1, int *tag2)
{
	get_operands(state, instr, op1, op2, tag1, tag2, 0);
}

int
commit(state_t *state) {
	// commits oldest instr (at head of ROB) into reg file or mem(for store instr)
	int reg_idx, is_int;
	ROB_t *cur_ROB = &state->ROB[state->ROB_head];

	dprintf(" [C]\n");

	if (state->ROB_head == state->ROB_tail)
		return 0;

	if (cur_ROB->completed) {
		reg_idx = get_dest_reg_idx(cur_ROB->instr, &is_int);
		if (reg_idx != -1) {
			if (is_int) {
				if (state->rf_int.tag[reg_idx] == state->ROB_head) {
					state->rf_int.reg_int.integer[reg_idx].w = cur_ROB->result.integer.w;
					state->rf_int.tag[reg_idx] = -1;
				}
			} else {
				if (state->rf_fp.tag[reg_idx] == state->ROB_head) {
					state->rf_fp.reg_fp.flt[reg_idx] = cur_ROB->result.flt;
					state->rf_fp.tag[reg_idx] = -1;
				}
			}
			state->ROB_head = (state->ROB_head + 1) % ROB_SIZE;
		}
	}

}


void
writeback(state_t *state) {

	int i, cq, iq, rob, val, is_int, reg_idx, ok;
	CQ_t *cur_CQ;
	IQ_t *cur_IQ;
	ROB_t *cur_ROB;

	dprintf(" [W]\n");
	// scan wb_port_int & wb_port_fp & branch_tag for complete instr.
	// write result into ROB
	// ROB stores complete instr as they enter IQ and CQ from dispatch stage

	//For each integer instruction, find
	//its corresponding entry in the ¡§ROB¡¨ (the tag field in wb port int specifies the index in the
	//¡§ROB¡¨ where the instruction resides). Set the completed flag to TRUE. Remember, the result of
	//the instruction was already computed when you issued the instruction (see Section 4.3); you are
	//only marking the completion of the instruction.
	//One exception is the ADDI portion of a load or
	//store instruction. The writeback of this part should not set the completed flag to TRUE because
	//only the effective address has completed; you should set the completed flag to TRUE for loads and
	//stores only when the entire memory operation has finished.
	for (i = 0; i < state->wb_port_int_num; i++) {
		if (state->wb_port_int[i].tag == -1)
			continue;

		// only mem (store/load)'s tag might >= ROB_SIZE
		if (state->wb_port_int[i].tag >= ROB_SIZE) 
			continue;

		state->ROB[state->wb_port_int[i].tag].completed = TRUE;
	}

	//If a match is found, deposit the result
    //of the instruction into the appropriate operand field in the IQ or CQ, and set the corresponding
    //tag field to -1, signaling to the issue stage that this operand is now ready.
	for (i = 0; i < state->wb_port_int_num; i ++) {
		if (state->wb_port_int[i].tag == -1)
			continue;
		for (cq = state->CQ_head; cq < state->CQ_tail; cq ++) {
			cur_CQ = &state->CQ[cq];
			ok = 0;
			if (cur_CQ->tag1 == state->wb_port_int[i].tag) {
				if (cur_CQ->tag1 > ROB_SIZE) {
					ok = 1;
				} else {
					int instr = cur_CQ->instr;
					int dst_idx, is_int;
					dst_idx = get_dest_reg_idx(cur_CQ->instr, &is_int);
					if (is_int) {
						if (state->rf_int.tag[dst_idx] == -1)
							ok = 1;
					}
				}
				if (ok) {
					// save "original tag" in "result" for mem_disambiguation to issue 
					cur_CQ->result.integer.w = cur_CQ->tag1 - ROB_SIZE;
					cur_CQ->tag1 = -1;
				}
			}
			ok = 0;
			if (cur_CQ->tag2 == state->wb_port_int[i].tag) {
				if (cur_CQ->tag2 > ROB_SIZE) {
					ok = 1;
				} else {
					int instr = cur_CQ->instr;
					int dst_idx, is_int;
					dst_idx = get_dest_reg_idx(cur_CQ->instr, &is_int);
					if (is_int) {
						if (state->rf_int.tag[dst_idx] == -1)
							ok = 1;
					}
				}
				if (ok) {
					// save "original tag" in "result" for mem_disambiguation to issue 
					cur_CQ->result.integer.w = cur_CQ->tag2 - ROB_SIZE;
					cur_CQ->tag2 = -1;
				}
			}
		}
		for (iq = state->IQ_head; iq < state->IQ_tail; iq++) {
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

	// clear tag
	for (i = 0; i < state->wb_port_int_num; i++) {
		if (state->wb_port_int[i].tag != -1)
			state->wb_port_int[i].tag = -1;
	}

}


void
execute(state_t *state) {
	int branch_tag;
	operand_t op1, op2, result;
	int tag1, tag2, use_imm, i, val;
	const op_info_t *op_info;

	dprintf(" [E]\n");

	if (state->if_id.instr == 0)
		return;

	// update ROB target & result by perform operation (id fetch_locked, instr is garbage)
	if (state->fetch_lock == FALSE) {
		get_operands_without_register_renaming(state, state->if_id.instr, &op1, &op2, &tag1, &tag2);
		perform_operation(state->if_id.instr, op1, op2, &result);
		op_info = decode_instr(state->if_id.instr, &use_imm);
		if (op_info->fu_group_num == FU_GROUP_MEM) {
			state->ROB[state->ROB_tail].target = result;
			state->ROB[state->ROB_tail].result = result;
		} else {
			state->ROB[state->ROB_tail].result = result;
		}
	}
	// advance function unit
	advance_fu_int(state->fu_int_list, state->wb_port_int, state->wb_port_int_num, &branch_tag);
	advance_fu_fp(state->fu_add_list, state->wb_port_fp, state->wb_port_fp_num);
	advance_fu_fp(state->fu_mult_list, state->wb_port_fp, state->wb_port_fp_num);
	advance_fu_fp(state->fu_div_list, state->wb_port_fp, state->wb_port_fp_num);
	advance_fu_mem(state->fu_mem_list, state->wb_port_int, state->wb_port_int_num, state->wb_port_fp, state->wb_port_fp_num);

	// update result right after execution for ROB which has been stalled
	for (i = 0; i < state->wb_port_int_num; i ++) {
		int rob = state->wb_port_int[i].tag;
		int instr = state->ROB[rob].instr;
		op_info = decode_instr(instr, &use_imm);

		if (rob == -1)
			continue;

		if (op_info->fu_group_num == FU_GROUP_MEM) {
			// a mem fu from store or load just finished
			switch(op_info->data_type) {
			case DATA_TYPE_W:
				switch(op_info->operation) {
				case OPERATION_LOAD://LW
					state->ROB[rob].result.integer.wu = load_4bytes(state->mem, state->ROB[rob].result.integer.w);
					//state->rf_int.reg_int.integer[FIELD_R2(instr)].wu = state->ROB[rob].result.integer.wu;
					break;
				case OPERATION_STORE://SW
					//store_4bytes(mem, result.integer.wu, rf_int->reg_int[r2].wu);
					break;
				}
				break;
			case DATA_TYPE_F:
				switch(op_info->operation) {
				case OPERATION_LOAD://L.S
					val = load_4bytes(state->mem, state->ROB[rob].result.integer.w);
					state->ROB[rob].result.flt = *(float *)(&val);
					//state->rf_fp.reg_fp.flt[FIELD_R2(instr)] = state->ROB[rob].result.flt;
					break;
				case OPERATION_STORE://S.S
					//store_4bytes(mem, result.integer.wu, *(int *)&rf_fp->reg_fp[r2]);
					break;
				}
			}
		} else {
			// other fu from just finished
			get_operands_without_register_renaming(state, instr, &op1, &op2, &tag1, &tag2);
			perform_operation(instr, op1, op2, &result);
			state->ROB[rob].result = result;
		}
	}

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

	int cq, cq2, is_int, issue = 0;
	CQ_t *cur_CQ, *cur_CQ2;

	dprintf(" [M]\n");

	for (cq = state->CQ_head; cq < state->CQ_tail; cq ++) {
		// not ready, try next
		cur_CQ = &state->CQ[cq];
		if ((cur_CQ->tag1 != -1) || (cur_CQ->tag2 != -1))
			continue;
		if (cur_CQ->store) {
			// store
			issue = 1; // issue immediately
			break;
		} else {
			// load
			// check with an order load
			int conflict = 0;
			for (cq2 = state->CQ_head; cq2 < state->CQ_tail; cq2 ++) {
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
		int issue_ret;
		// get is_int
		get_dest_reg_idx(cur_CQ->instr, &is_int);
		// issue
		issue_ret = issue_fu_mem(state->fu_mem_list, cur_CQ->result.integer.w, !is_int, cur_CQ->store);
		// issue at most one instruction
		if (issue_ret == 0) {
			cur_CQ->issued = TRUE;
			state->CQ_head = (state->CQ_head + 1) % CQ_SIZE;
			//if (cur_CQ->store) {
				//state->ROB[cur_CQ->tag1].completed = TRUE; // need to be verified
			//}
		}
	}
}


int
issue(state_t *state) {
	int use_imm, issue_ret = -1, iq, cq, tag = -1, instr;
	const op_info_t *op_info;

	// scan IQ every cycle, search ready instr to issue to fu
	// ready for issue when both operands available
	// IQ should include both int & fp
	dprintf(" [I]\n");

	for (iq = state->IQ_head; iq < state->IQ_tail; iq ++) {
		if ((state->IQ[iq].tag1 == -1) && (state->IQ[iq].tag2 == -1)) {
			instr = state->IQ[iq].instr;
			op_info = decode_instr(instr, &use_imm);
			// get tag
			if (op_info->fu_group_num == FU_GROUP_MEM) {
				for (cq = state->CQ_head; cq < state->CQ_tail; cq ++) {
					if (instr == state->CQ[cq].instr)
						tag = state->CQ[cq].tag1;
				}
			} else {
				tag = state->IQ[iq].ROB_index;
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
			// issue at most one instruction? no
			if (issue_ret == 0) {
				dprintf(" [I] IQ %d issued = TRUE\n", iq);
				state->IQ[iq].issued = TRUE;
				//state->IQ_head ++; //TBD (should it be compact?)
				//break;
			}
		}
	}

	// adjust IQ_head
	for (iq = state->IQ_head; iq < state->IQ_tail; iq ++) {
		if (state->IQ[iq].issued) {
			state->IQ_head = (state->IQ_head + 1) % IQ_SIZE;
		} else {
			break;
		}
	}

}


int
dispatch(state_t *state) {

	int use_imm, tag1, tag2, iq, is_int_cq, is_int_rob, dst_reg_cq, dst_reg_rob, rob;
	const op_info_t *op_info;
	ROB_t *cur_ROB;
	IQ_t *cur_IQ;
	CQ_t *cur_CQ;
	operand_t operand1, operand2;

	dprintf(" [D]\n");

	op_info = decode_instr(state->if_id.instr, &use_imm);
	if (op_info->fu_group_num == FU_GROUP_NONE) // NOP
		return 0;

	// A. move instr from if_id to IQ and ROB, or CQ
	// for NOP, no move
	// for non-mem instr: put in IQ and ROB only
	// for mem instr: split into two instr
	//     (1) compute effect addr (reg+imm) : placed in IQ
	//     (2) use computed addr to access mem : placed in CQ
	// IQ,ROB,CQ: circular queues, head(instr to enter), tail(instr to leave)

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
			for (rob = state->ROB_head; rob < state->ROB_tail; rob ++) {
				dst_reg_rob = get_dest_reg_idx(state->ROB[rob].instr, &is_int_rob);
				// check if store dst reg is in-flight (in ROBs)
				if ((is_int_cq == is_int_rob) && (dst_reg_cq == dst_reg_rob)) {
					cur_CQ->tag2 = rob;
				}
			}
		}
		state->CQ_tail = (state->CQ_tail+1) % CQ_SIZE;
	}

	// HALT should not put in IQ.
	if (op_info->fu_group_num != FU_GROUP_HALT) {

		// in IQ: (instr should be inserted at tail, IQ_tail=(IQ_tail+1)%IQ_SIZE)
		//   instr, pc: no problem
		//   issued: issued or not (should init as FALSE)
		//   ROB_index: location where instr was inserted
		cur_IQ = &state->IQ[state->IQ_tail];
		cur_IQ->instr = state->if_id.instr;
		cur_IQ->pc = state->pc;
		cur_IQ->issued = FALSE;
		cur_IQ->ROB_index = state->ROB_tail;
		get_operands_with_register_renaming(state, cur_IQ->instr, &operand1, &operand2, &tag1, &tag2);
		cur_IQ->operand1 = operand1;
		cur_IQ->operand2 = operand2;
		cur_IQ->tag1 = tag1;//v TBD (need to assign waited in-flight instr idx in ROB)
		cur_IQ->tag2 = tag2;//v TBD (need to assign waited in-flight instr idx in ROB)
		state->IQ_tail = (state->IQ_tail+1) % IQ_SIZE;
	}
    // B. register renaming
	//    by matching tag in IQ,CQ & tag in int/fp reg files
	//    tag:     -1: value is ready and present
	//         not -1: being computed by an in-flight instr (value:0 ~ ROB_SIZE-1)
	//                 indicates the index in ROB where in-flight instr is
	//    <procedure>
	//    1. To rename "source" register, check desired register 
	//       if reg.tag==-1, read reg value into operand of IQ, set operand.tag to -1
	//       if reg.tag!=-1, there is a in-flight instr producing desired value.
	//                       check in-flight instr in ROB's completed flag
	//                       if complete, but not yet commit,copy result to operand of IQ, set operand.tag to -1
	//                       if not complete, no operand is available, set operand.tag of IQ to ROB_index of in-flight instr.
	//                                        this will stall IQ entry until the in-flight complete and providing missing reg val

	//    2. To rename "destination" register,
	//       set dest reg's tag in reg file to ROB_index of the dispached instr.
	//    3. incerment IQ_tail%IQ_SIZE


	// in ROB:
	//  instr: instruction
	//  completed: in dispatch always FALSE, except "halt"
	//  result: uninitialized here, filled after exec and writeback
	//  target: uninitialized here, known after exec
	//           used for contrl instr: instr's target addr 
	//                                  (link addr for jump-and-link instr, or "taken or not taken" for cond. branch)
	//           and for mem instr: effective addr
	// => ROB_tail=(ROB_tail+1) mode ROB_SIZE
	cur_ROB = &state->ROB[state->ROB_tail];
	cur_ROB->instr = state->if_id.instr; //TBD:execpt halt
	if (op_info->fu_group_num == FU_GROUP_HALT)
		cur_ROB->completed = TRUE;
	else
		cur_ROB->completed = FALSE;
	//cur_ROB->result = NULL;
	if (op_info->fu_group_num == FU_GROUP_BRANCH) {
		if ((op_info->operation == OPERATION_JAL) ||
			(op_info->operation == OPERATION_JALR))
		cur_ROB->target = *(operand_t *)&state->pc;
	} else {
		//cur_ROB->target = NULL;
	}
	state->ROB_tail = (state->ROB_tail+1) % ROB_SIZE;

	// check if HALT is found, HALT take effect only when branch is not taken in writeback
	if (op_info->fu_group_num == FU_GROUP_HALT) {
		//if (state->branch != TAKEN) {
			state->fetch_lock = HALT;
			//dprintf("   [D]:Halt found => fetch_lock = HALT\n");
		//}
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
