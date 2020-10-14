class ltcl_string_map definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    types:
      begin of ty_struc,
        a type string,
        b type abap_bool,
        c type i,
      end of ty_struc.
    DATA mo_map TYPE REF TO zcl_abap_string_map.

    METHODS setup.
    METHODS teardown.
    methods assert_create IMPORTING iv_from TYPE any
                                    iv_exp TYPE string.
    methods assert_from IMPORTING iv_from TYPE any.
    METHODS assert_read_only_exception IMPORTING exc TYPE REF TO cx_root.

    methods get_set_has for testing.
    methods size_empty_clear for testing.
    methods delete for testing.
    methods keys_values for testing.
    methods to_struc for testing.
    methods from_struc for testing.
    methods strict for testing.
    methods from_to_struc_negative for testing.
    methods from_entries for testing.
    methods freeze for testing.
    methods create_from for testing.
    methods case_insensitive for testing.

endclass.

class ltcl_string_map implementation.

  method setup.
    mo_map = zcl_abap_string_map=>create( ).
  ENDMETHOD.

  method teardown.
    FREE mo_map.
  ENDMETHOD.

  method assert_create.
    try.
      zcl_abap_string_map=>create( iv_from = iv_from ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into DATA(lx).
      cl_abap_unit_assert=>assert_equals(
        exp = iv_exp
        act = lx->get_text( ) ).
    endtry.
  ENDMETHOD.

  method assert_from.
    DATA(lo_cut) = zcl_abap_string_map=>create( iv_from = iv_from ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals( exp = '1'
                                        act = lo_cut->get( 'A' ) ).
  ENDMETHOD.

  method assert_read_only_exception.
     cl_abap_unit_assert=>assert_equals(
        exp = 'String map is read only'
        act = exc->get_text( ) ).
  ENDMETHOD.

  method create_from.

    assert_create( iv_from = `abc`
                   iv_exp = 'Incorrect input for string_map=>create, typekind g' ).

    assert_create( iv_from = me
                   iv_exp =  'Incorrect string map instance to copy from' ).

    data: begin of ls_dummy,
            a type string value '1',
          end of ls_dummy.

    mo_map->set( iv_key = 'A'
                 iv_val = '1' ).

    assert_from( mo_map ).                  " From object

    assert_from( mo_map->mt_entries ).      " From int. table

    assert_from( ls_dummy ).                " From struc
  endmethod.

  method freeze.
    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    lo_cut->set( iv_key = 'A'
                 iv_val = 'avalue' )->freeze( ).
    try.
      lo_cut->set( iv_key = 'A'
                   iv_val = '2' ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into DATA(lx).
      assert_read_only_exception( lx ).
    endtry.

    try.
      lo_cut->set( iv_key = 'B'
                   iv_val = '2' ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      assert_read_only_exception( lx ).
    endtry.

    try.
      lo_cut->delete( 'A' ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      assert_read_only_exception( lx ).
    endtry.

    try.
      lo_cut->clear( ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      assert_read_only_exception( lx ).
    endtry.

    data lt_entries type STANDARD TABLE OF zcl_abap_string_map=>ts_entry.
    try.
      lo_cut->from_entries( lt_entries ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      assert_read_only_exception( lx ).
    endtry.

    data ls_dummy type syst.
    try.
      lo_cut->from_struc( ls_dummy ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      assert_read_only_exception( lx ).
    endtry.

  endmethod.

  method get_set_has.

    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = ''
      act = lo_cut->get( 'C' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'C' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->has( 'A' ) ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'newvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'newvalue'
      act = lo_cut->get( 'A' ) ).

  endmethod.

  method size_empty_clear.

    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->is_empty( ) ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).

    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'newvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->is_empty( ) ).

    lo_cut->clear( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->is_empty( ) ).

  endmethod.

  method delete.

    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    lo_cut->delete( iv_key = 'A' ).

    cl_abap_unit_assert=>assert_equals(
      exp = ''
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

  endmethod.

  method keys_values.

    data lo_cut type ref to zcl_abap_string_map.
    data lt_exp type string_table.
    lo_cut = zcl_abap_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    clear lt_exp.
    append 'A' to lt_exp.
    append 'B' to lt_exp.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp
      act = lo_cut->keys( ) ).

    clear lt_exp.
    append 'avalue' to lt_exp.
    append 'bvalue' to lt_exp.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp
      act = lo_cut->values( ) ).

  endmethod.

  method to_struc.

    data ls_struc_act type ty_struc.
    data ls_struc_exp type ty_struc.
    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    lo_cut->set(
      iv_key = 'a'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'X' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '123' ).

    lo_cut->to_struc( changing cs_container = ls_struc_act ).

    ls_struc_exp-a = 'avalue'.
    ls_struc_exp-b = abap_true.
    ls_struc_exp-c = 123.

    cl_abap_unit_assert=>assert_equals(
      exp = ls_struc_exp
      act = ls_struc_act ).

  endmethod.

  method from_struc.

    data ls_struc type ty_struc.
    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    ls_struc-a = 'avalue'.
    ls_struc-b = abap_true.
    ls_struc-c = 123.

    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    lo_cut->from_struc( ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'X'
      act = lo_cut->get( 'B' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '123'
      act = lo_cut->get( 'C' ) ).

  endmethod.

  method strict.

    data ls_struc_act type ty_struc.
    data ls_struc_exp type ty_struc.
    data lx type ref to cx_root.
    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    lo_cut->set(
      iv_key = 'a'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'X' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '123' ).
    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    ls_struc_exp-a = 'avalue'.
    ls_struc_exp-b = abap_true.
    ls_struc_exp-c = 123.

    try.
      lo_cut->to_struc( changing cs_container = ls_struc_act ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'Component Z not found in target'
        act = lx->get_text( ) ).
    endtry.

    lo_cut->strict( abap_false )->to_struc( changing cs_container = ls_struc_act ).

    cl_abap_unit_assert=>assert_equals(
      exp = ls_struc_exp
      act = ls_struc_act ).

  endmethod.

  method from_to_struc_negative.

    data lt_dummy type string_table.
    data lx type ref to cx_root.
    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    try.
      lo_cut->from_struc( lt_dummy ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'Only structures supported'
        act = lx->get_text( ) ).
    endtry.

    try.
      lo_cut->to_struc( changing cs_container = lt_dummy ).
      cl_abap_unit_assert=>fail( ).
    catch cx_root into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'Only structures supported'
        act = lx->get_text( ) ).
    endtry.

  endmethod.

  method from_entries.

    types:
      begin of lty_pair,
        key type string,
        val type string,
      end of lty_pair.

    data lt_entries type table of lty_pair.
    data ls_entry like line of lt_entries.
    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( ).

    ls_entry-key = 'A'.
    ls_entry-val = 'avalue'.
    append ls_entry to lt_entries.

    ls_entry-key = 'B'.
    ls_entry-val = '123'.
    append ls_entry to lt_entries.

    lo_cut->from_entries( lt_entries ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '123'
      act = lo_cut->get( 'B' ) ).

  endmethod.

  method case_insensitive.

    data lo_cut type ref to zcl_abap_string_map.
    lo_cut = zcl_abap_string_map=>create( iv_case_insensitive = abap_true ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'a' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'b' ) ).

    data lt_exp_keys type string_table.
    append 'A' to lt_exp_keys.
    append 'B' to lt_exp_keys.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp_keys
      act = lo_cut->keys( ) ).

  endmethod.

endclass.
