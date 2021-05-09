CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      BEGIN OF gcs_toolbar,
        expall_name     TYPE salv_de_function VALUE 'EXPALL',
        expall_icon     TYPE iconname VALUE icon_expand_all,
        colall_name     TYPE salv_de_function VALUE 'COLALL',
        colall_icon     TYPE iconname VALUE icon_collapse_all,
        expall_tooltip  TYPE string,
        colall_tooltip  TYPE string,
        expall_position TYPE salv_de_function_pos VALUE if_salv_c_function_position=>right_of_salv_functions,
        colall_position TYPE salv_de_function_pos VALUE if_salv_c_function_position=>right_of_salv_functions,
      END OF gcs_toolbar.

    CLASS-METHODS:
      class_constructor,

      get_icon
        IMPORTING
          iv_type        TYPE char1
        RETURNING
          VALUE(rv_icon) TYPE text40.

    METHODS:
      handle_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
          e_salv_function
          sender,
      handle_link_click     FOR EVENT link_click   OF cl_salv_events_table
        IMPORTING
          row
          column
          sender.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.

  METHOD class_constructor.
    gcs_toolbar = VALUE #( BASE gcs_toolbar
                           expall_tooltip = 'Expand all Details'(e01)
                           colall_tooltip = 'Collapse all Details'(c01) ).
  ENDMETHOD.


  METHOD handle_added_function.
    TRY.
        DATA(lo_filters) = go_salv_table->get_filters( ).
        CASE e_salv_function.
          WHEN gcs_toolbar-expall_name.
            LOOP  AT gt_tadir_output ASSIGNING FIELD-SYMBOL(<ls_tadir_output>)
                  WHERE expand(3) EQ icon_expand(3).
              DATA(lv_add_subrows_index) = sy-tabix + 1.
              <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).
              INSERT LINES OF VALUE zkco_samples_salv_tadir_out_t(
                  FOR ls_tadir IN gt_tadir
                  WHERE ( pgmid  EQ <ls_tadir_output>-pgmid AND
                          object EQ <ls_tadir_output>-object )
                  ( CORRESPONDING #( ls_tadir ) ) )
                INTO gt_tadir_output
                INDEX lv_add_subrows_index.
            ENDLOOP.
            IF sy-subrc EQ 0.
              DATA(lv_refresh_alv) = abap_true.
            ENDIF.
          WHEN gcs_toolbar-colall_name.
            LOOP  AT gt_tadir_output ASSIGNING <ls_tadir_output>
                  WHERE expand(3) EQ icon_collapse(3).
              <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
              DELETE gt_tadir_output  WHERE pgmid  EQ <ls_tadir_output>-pgmid
                                        AND object EQ <ls_tadir_output>-object
                                        AND expand IS INITIAL.
            ENDLOOP.
            IF sy-subrc EQ 0.
              lv_refresh_alv = abap_true.
            ENDIF.
        ENDCASE.
      CATCH cx_salv_not_found.
      CATCH cx_salv_data_error.
      CATCH cx_salv_existing.
    ENDTRY.
    IF lv_refresh_alv EQ abap_true.
      go_salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.



  METHOD get_icon.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = SWITCH #( iv_type WHEN 'E' THEN icon_expand
                                                  WHEN 'C' THEN icon_collapse )
        info                  = SWITCH text40( iv_type WHEN 'E' THEN 'Expand Details'(e02)
                                                       WHEN 'C' THEN 'Collapse Details'(c02) )
        add_stdinf            = ' '
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 0
        outputfield_too_short = 0
        OTHERS                = 0.
  ENDMETHOD.

  METHOD handle_link_click.
    CASE column.
      WHEN 'EXPAND'.
        ASSIGN gt_tadir_output[ row ] TO FIELD-SYMBOL(<ls_tadir_output>).
        IF <ls_tadir_output>-expand(3) EQ icon_expand(3).
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).
          DATA(lv_add_subrows_index) = row + 1.
          INSERT LINES OF VALUE zkco_samples_salv_tadir_out_t(
              FOR ls_tadir IN gt_tadir
              WHERE ( pgmid  EQ <ls_tadir_output>-pgmid AND
                      object EQ <ls_tadir_output>-object )
              ( CORRESPONDING #( ls_tadir ) ) )
            INTO gt_tadir_output
            INDEX lv_add_subrows_index.
        ELSE.
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
          DELETE gt_tadir_output  WHERE pgmid  EQ <ls_tadir_output>-pgmid
                                    AND object EQ <ls_tadir_output>-object
                                    AND expand IS INITIAL.
        ENDIF.

        go_salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
