
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
get_idx_by_match_ROB_with_dest_reg(state_t *state, int src_is_int, int src_reg_idx)
{
	int dst_is_int, dst_reg_idx, i;

	for (i=state->ROB_head; i<state->ROB_tail; i++) {
		dst_reg_idx = get_dest_reg_idx(state->ROB[i].instr, &dst_is_int);
		if ((src_is_int == dst_is_int) && (src_reg_idx == dst_reg_idx))
			return i;
	}

	return -1;
}

void
get_operands(state_t *state, operand_t *op1, operand_t *op2, int *tag1, int *tag2, int reg_renaming)
{
	const op_info_t *op_info;
	int use_imm = 0;
	unsigned int r1 = 0, r2 = 0, r3 = 0, imm = 0;

	int instr = state->if_id.instr;
	rf_int_t *rf_int = &state->rf_int;
	rf_fp_t *rf_fp = &state->rf_fp;

	op_info = decode_instr(instr, &use_imm);
	r1 = FIELD_R1(instr);
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
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1);
					rf_int->tag[r1] = *tag1;
				}
				break;
			case FU_GROUP_MEM:
				*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
				*op2 = *(operand_t *)&(imm);
				if (reg_renaming && (r1 != 0)) {
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1);
					rf_int->tag[r1] = *tag1;
				}
				break;
			case FU_GROUP_BRANCH:
				switch(op_info->operation) {
				case OPERATION_J:
					break;
				case OPERATION_JR:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1);
						rf_int->tag[r1] = *tag1;
					}
				case OPERATION_JAL:
					break;
				case OPERATION_JALR:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1);
						rf_int->tag[r1] = *tag1;
					}
					break;
				case OPERATION_BEQZ:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1);
						rf_int->tag[r1] = *tag1;
					}
					break;
				case OPERATION_BNEZ:
					*op1 = *(operand_t *)&rf_int->reg_int.integer[r1];
					if (reg_renaming && (r1 != 0)) {
						*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1);
						rf_int->tag[r1] = *tag1;
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
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 1, r1);
					rf_int->tag[r1] = *tag1;
				}
				if (reg_renaming && (r2 != 0)) {
					*tag2 = get_idx_by_match_ROB_with_dest_reg(state, 1, r2);
					rf_int->tag[r2] = *tag2;
				}
				break;
			case FU_GROUP_ADD:
			case FU_GROUP_MULT:
			case FU_GROUP_DIV:
				*op1 = *(operand_t *)&rf_fp->reg_fp.flt[r1];
				*op2 = *(operand_t *)&rf_fp->reg_fp.flt[r2];
				if (reg_renaming) {
					*tag1 = get_idx_by_match_ROB_with_dest_reg(state, 0, r1);
					*tag2 = get_idx_by_match_ROB_with_dest_reg(state, 0, r2);
					rf_fp->tag[r1] = *tag1;
					rf_fp->tag[r2] = *tag2;
				}
				break;
			}
		}
	}
}

void
get_operands_with_register_renaming(state_t *state, operand_t *op1, operand_t *op2, int *tag1, int *tag2)
{
	get_operands(state, op1, op2, tag1, tag2, 1);
}

void
get_operands_without_register_renaming(state_t *state, operand_t *op1, operand_t *op2, int *tag1, int *tag2)
{
	get_operands(state, op1, op2, tag1, tag2, 0);
}

int
commit(state_t *state) {
	// commits oldest instr (at head of ROB) into reg file or mem(for store instr)
}


void
writeback(state_t *state) {
	// scan wb_port_int & wb_port_fp & branch_tag for complete instr.
	// write result into ROB
	// ROB stores complete instr as they enter IQ and CQ from dispatch stage
}


void
execute(state_t *state) {
	int branch_tag;

	operand_t op1, op2, result;
	int tag1, tag2, branch, use_imm;
	const op_info_t *op_info;

	// update ROB target & result by perform operation
	get_operands_without_register_renaming(state, &op1, &op2, &tag1, &tag2);
	perform_operation(state->if_id.instr, op1, op2, &result);
	op_info = decode_instr(state->if_id.instr, &use_imm);
	if (op_info->fu_group_num == FU_GROUP_MEM)
		state->ROB[state->ROB_tail].target = result;
	else
		state->ROB[state->ROB_tail].result = result;

	// advance function unit
	advance_fu_int(state->fu_int_list, state->wb_port_int, state->wb_port_int_num, &branch_tag);
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
}


int
issue(state_t *state) {
	int use_imm, issue_ret = -1, iq, cq, tag = -1, instr, is_int, reg_idx;
	const op_info_t *op_info;

	// scan IQ every cycle, search ready instr to issue to fu
	// ready for issue when both operands available
	// IQ should include both int & fp

	for (iq=state->IQ_head; iq<state->IQ_tail; iq++) {
		if ((state->IQ[iq].tag1 == -1) && (state->IQ[iq].tag2 == -1)) {
			instr = state->IQ[iq].instr;
			for (cq=state->CQ_head; cq<state->CQ_tail; cq++) {
				if (instr == state->CQ[cq].instr)
					tag = state->CQ[cq].tag1;
			}
			//reg_idx = get_dest_reg_idx(instr, &is_int);
			//tag = is_int ? state->rf_int.tag[reg_idx] : state->rf_fp.tag[reg_idx];
			op_info = decode_instr(instr, &use_imm);
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
				state->IQ[iq].issued = TRUE;
				state->IQ_head ++; //TBD (should it be compact?)
				break;
			}
		}
	}


}


int
dispatch(state_t *state) {

	int use_imm, tag1, tag2;
	const op_info_t *op_info;
	ROB_t *cur_ROB;
	IQ_t *cur_IQ;
	CQ_t *cur_CQ;
	operand_t operand1, operand2;

	op_info = decode_instr(state->if_id.instr, &use_imm);

	// all instr, except NOP and HALT, should put in IQ.
	if ((op_info->fu_group_num == FU_GROUP_NONE) || // NOP
		(op_info->fu_group_num == FU_GROUP_HALT))   // HALT
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
		cur_CQ->tag1 = state->IQ_tail + IQ_BASE;

		cur_CQ->result; //?
		if (op_info->operation == OPERATION_LOAD)
			cur_CQ->tag2 = -1;
		else
			cur_CQ->tag2 = 0; //TBD
		state->CQ_tail = (state->CQ_tail+1) % CQ_SIZE;
	}


	// in IQ: (instr should be inserted at tail, IQ_tail=(IQ_tail+1)%IQ_SIZE)
	//   instr, pc: no problem
	//   issued: issued or not (should init as FALSE)
	//   ROB_index: location where instr was inserted
	cur_IQ = &state->IQ[state->IQ_tail];
	cur_IQ->instr = state->if_id.instr;
	cur_IQ->pc = state->pc;
	cur_IQ->issued = FALSE;
	cur_IQ->ROB_index = state->ROB_tail;
	get_operands_with_register_renaming(state, &operand1, &operand2, &tag1, &tag2);
	cur_IQ->operand1 = operand1;
	cur_IQ->operand2 = operand2;
	cur_IQ->tag1 = tag1;//v TBD (need to assign waited in-flight instr idx in ROB)
	cur_IQ->tag2 = tag2;//v TBD (need to assign waited in-flight instr idx in ROB)
	state->IQ_tail = (state->IQ_tail+1) % IQ_SIZE;


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

	// register renaming





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

}


void
fetch(state_t *state) {

	// update pc, if_id
	int pc = state->pc;
	int instr;

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
	dprintf("   [F][pos]:pc+4=0x%X\n",pc);

}
