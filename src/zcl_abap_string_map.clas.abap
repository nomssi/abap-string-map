CLASS zcl_abap_string_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS version TYPE string VALUE 'v1.0.2a'.

    TYPES:
      BEGIN OF ts_entry,
        k TYPE string,
        v TYPE string,
      END OF ts_entry.
    TYPES tt_entries TYPE SORTED TABLE OF ts_entry WITH UNIQUE KEY k.

    DATA mt_entries TYPE tt_entries READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        !iv_case_insensitive TYPE abap_bool DEFAULT abap_false
        !iv_from             TYPE any OPTIONAL
      RETURNING
        VALUE(ro_instance)   TYPE REF TO zcl_abap_string_map.
    METHODS constructor
      IMPORTING
        !iv_case_insensitive TYPE abap_bool DEFAULT abap_false
        !iv_from TYPE any OPTIONAL.

    METHODS get
      IMPORTING
        !iv_key TYPE string
      RETURNING
        VALUE(rv_val) TYPE string.
    METHODS has
      IMPORTING
        !iv_key TYPE string
      RETURNING
        VALUE(rv_has) TYPE abap_bool.
    METHODS set
      IMPORTING
        !iv_key       TYPE string
        !iv_val       TYPE string
      RETURNING VALUE(ro_map) TYPE REF TO zcl_abap_string_map.
    METHODS size
      RETURNING VALUE(rv_size) TYPE i.
    METHODS is_empty
      RETURNING VALUE(rv_yes) TYPE abap_bool.
    METHODS delete
      IMPORTING !iv_key TYPE string.
    METHODS keys
      RETURNING VALUE(rt_keys) TYPE string_table.
    METHODS values
      RETURNING
        VALUE(rt_values) TYPE string_table.
    METHODS clear.
    METHODS to_struc
      CHANGING !cs_container TYPE any.
    METHODS from_struc
      IMPORTING !is_container TYPE any.
    METHODS from_entries
      IMPORTING !it_entries TYPE ANY TABLE.
    METHODS strict
      IMPORTING !iv_strict         TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_abap_string_map.
    METHODS freeze.

  PROTECTED SECTION.
    METHODS upcase
      IMPORTING !iv_key       TYPE string
      RETURNING VALUE(rv_key) TYPE string.

    METHODS check_mutable.
    CLASS-METHODS raise IMPORTING text TYPE string.
    CLASS-METHODS get_components IMPORTING is_container TYPE any
                                 RETURNING VALUE(rt_comp) TYPE ABAP_COMPDESCR_TAB.

  PRIVATE SECTION.
    DATA mv_is_strict TYPE abap_bool.
    DATA mv_read_only TYPE abap_bool.
    DATA mv_case_insensitive TYPE abap_bool.
ENDCLASS.

CLASS zcl_abap_string_map IMPLEMENTATION.

  METHOD clear.
    check_mutable(  ).
    CLEAR mt_entries.
  ENDMETHOD.

  METHOD constructor.
    mv_is_strict = abap_true.
    mv_case_insensitive = iv_case_insensitive.

    CHECK iv_from IS NOT INITIAL.
    DATA(lv_kind) = cl_abap_typedescr=>describe_by_data( iv_from )->type_kind.

    CASE lv_kind.
      WHEN cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2.
        from_struc( iv_from ).

      WHEN cl_abap_typedescr=>typekind_oref.
        TRY.
            mt_entries = CAST zcl_abap_string_map( iv_from )->mt_entries.
          CATCH cx_sy_move_cast_error.
            raise( 'Incorrect string map instance to copy from' ).
        ENDTRY.

      WHEN cl_abap_typedescr=>typekind_table.
        from_entries( iv_from ).

      WHEN OTHERS.
        raise( |Incorrect input for string_map=>create, typekind { lv_kind }| ).
    ENDCASE.
  ENDMETHOD.


  METHOD create.
    ro_instance = NEW #( iv_case_insensitive = iv_case_insensitive
                         iv_from             = iv_from ).
  ENDMETHOD.

  METHOD raise.
    lcx_error=>raise( text ).
  ENDMETHOD.

  METHOD check_mutable.
    CHECK mv_read_only = abap_true.
    raise( 'String map is read only' ).
  ENDMETHOD.

  METHOD delete.
    check_mutable( ).
    DELETE mt_entries WHERE k = iv_key.
  ENDMETHOD.

  METHOD freeze.
    mv_read_only = abap_true.
  ENDMETHOD.

  METHOD from_entries.
    FIELD-SYMBOLS <entry> TYPE ts_entry.

    check_mutable( ).

    LOOP AT it_entries ASSIGNING <entry> CASTING.
      set( iv_key = <entry>-k
           iv_val = <entry>-v ).
    ENDLOOP.
  ENDMETHOD.

  method get_components.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( is_container ).
    CASE lo_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2.
        rt_comp = CAST cl_abap_structdescr( lo_type )->components.
      WHEN OTHERS.
        raise( 'Only structures supported' ).
    ENDCASE.
  endmethod.

  METHOD from_struc.
    FIELD-SYMBOLS <val> TYPE any.

    clear( ).

    LOOP AT get_components( is_container ) ASSIGNING FIELD-SYMBOL(<c>)
      WHERE type_kind CO 'bsI8PaeFCNgXyDT'. " values
      ASSIGN COMPONENT <c>-name OF STRUCTURE is_container TO <val>.
      ASSERT sy-subrc = 0.
      set( iv_key = |{ <c>-name }|
           iv_val = |{ <val> }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD has.
    rv_has = xsdbool( line_exists( mt_entries[ k = iv_key ] ) ).
  ENDMETHOD.

  METHOD is_empty.
    rv_yes = xsdbool( lines( mt_entries ) = 0 ).
  ENDMETHOD.

  METHOD keys.
    rt_keys = VALUE #( FOR <entry> IN mt_entries ( <entry>-k ) ).
  ENDMETHOD.

  METHOD values.
    rt_values = VALUE #( FOR <entry> IN mt_entries ( <entry>-v ) ).
  ENDMETHOD.

  METHOD upcase.
    rv_key = SWITCH string( mv_case_insensitive WHEN abap_true THEN to_upper( iv_key ) ELSE iv_key ).
  ENDMETHOD.

  METHOD get.
    rv_val = VALUE #( mt_entries[ k = upcase( iv_key ) ]-v OPTIONAL ).
  ENDMETHOD.

  METHOD set.
    check_mutable( ).
    DATA(ls_entry) = VALUE ts_entry( k = upcase( iv_key )
                                     v = iv_val ).

    ASSIGN mt_entries[ k = ls_entry-k ] TO FIELD-SYMBOL(<entry>).
    IF sy-subrc = 0.
      <entry>-v = ls_entry-v.
    ELSE.
      INSERT ls_entry INTO TABLE mt_entries.
    ENDIF.

    ro_map = me.
  ENDMETHOD.

  METHOD size.
    rv_size = lines( mt_entries ).
  ENDMETHOD.

  METHOD strict.
    mv_is_strict = iv_strict.
    ro_instance = me.
  ENDMETHOD.

  METHOD to_struc.
    FIELD-SYMBOLS <val> TYPE any.

    get_components( cs_container ).

    LOOP AT mt_entries ASSIGNING FIELD-SYMBOL(<entry>).
      DATA(lv_field) = to_upper( <entry>-k ).
      ASSIGN COMPONENT lv_field OF STRUCTURE cs_container TO <val>.
      IF sy-subrc = 0.
        <val> = <entry>-v.      " TODO check target type ?
      ELSEIF mv_is_strict = abap_true.
        raise( |Component { lv_field } not found in target| ).
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
