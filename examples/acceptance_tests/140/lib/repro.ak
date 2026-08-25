use aiken/collection/list

use list_ext

pub type UtxoIndex =
  Int

@list
pub type UpsertLoanIndexer {
  pool_in_idx: UtxoIndex,
  pool_out_idx: Int,
  loan_idxs: List<TxType>,
  pool_param_ref_idx: Int,
}

pub type TxType {
  Create { hash_in_idx: Int, loan_out_idx: Int }
  Update {
    loan_in_idx: Int,
    loan_owner_idx: UtxoIndex,
    loan_out_idx: Int,
  }
  Close { loan_in_idx: Int }
}


test stdlib_any_matches_local_copy_and_expected_result() {
  let indexer =
    UpsertLoanIndexer {
      pool_in_idx: 0,
      pool_out_idx: 1,
      loan_idxs: [
        Create { hash_in_idx: 0, loan_out_idx: 1 },
        Update { loan_in_idx: 2, loan_owner_idx: 3, loan_out_idx: 4 },
        Close { loan_in_idx: 5 },
      ],
      pool_param_ref_idx: 6,
    }
  let predicate = fn(tx_type) {
    when tx_type is {
      Update { loan_in_idx, .. } | Close { loan_in_idx } -> loan_in_idx == 5
      _ -> False
    }
  }
  let local = list_ext.any(indexer.loan_idxs, predicate)
  let standard = list.any(indexer.loan_idxs, predicate)
  let expected = True
  standard == local && local == expected
}
