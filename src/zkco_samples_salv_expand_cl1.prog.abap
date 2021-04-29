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
          value(rv_icon) TYPE text40.

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
    gcs_toolbar-expall_tooltip = 'Expand all Details'(e01).
    gcs_toolbar-colall_tooltip = 'Collapse all Details'(c01).
  ENDMETHOD.


  METHOD handle_added_function.
    DATA:
      lv_count       TYPE i,
      lv_tabix       TYPE i,
      lv_refresh_alv TYPE abap_bool.

    DATA:
      ls_stable TYPE lvc_s_stbl.

    DATA:
      lr_tadir TYPE REF TO zkco_samples_salv_tadir.

    DATA:
      lo_filters TYPE REF TO cl_salv_filters.

    FIELD-SYMBOLS:
      <ls_tadir_output>     TYPE zkco_samples_salv_tadir_output,
      <ls_tadir_output_new> TYPE zkco_samples_salv_tadir_output.

    TRY.
        lo_filters = go_salv_table->get_filters( ).
        CASE e_salv_function.
          WHEN gcs_toolbar-expall_name.
            LOOP  AT gt_tadir_output ASSIGNING <ls_tadir_output>
                  WHERE expand(3) EQ icon_expand(3).
              lv_tabix = sy-tabix.
              <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).
              CLEAR: lv_count.
              LOOP  AT gt_tadir REFERENCE INTO lr_tadir
                    WHERE pgmid  EQ <ls_tadir_output>-pgmid
                      AND object EQ <ls_tadir_output>-object.
                lv_count = lv_count + 1.
                INSERT INITIAL LINE INTO gt_tadir_output INDEX lv_tabix + lv_count ASSIGNING <ls_tadir_output_new>.
                MOVE-CORRESPONDING lr_tadir->* TO <ls_tadir_output_new>.
              ENDLOOP.
            ENDLOOP.
            IF sy-subrc EQ 0.
              lv_refresh_alv = abap_true.
            ENDIF.
          WHEN gcs_toolbar-colall_name.
            LOOP  AT gt_tadir_output ASSIGNING <ls_tadir_output>
                  WHERE expand(3) EQ icon_collapse(3).
              <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
              DELETE gt_tadir_output  FROM sy-tabix + 1
                                      WHERE pgmid  EQ <ls_tadir_output>-pgmid
                                        AND object EQ <ls_tadir_output>-object.
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
    DATA:
      lv_name TYPE iconname,
      lv_info TYPE text40.
    CASE iv_type.
      WHEN 'E'.
        lv_name = icon_expand.
        lv_info = 'Expand Details'(e02).
      WHEN 'C'.
        lv_name = icon_collapse.
        lv_info = 'Collapse Details'(c02).
    ENDCASE.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = lv_name
        info                  = lv_info
        add_stdinf            = ' '
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 0
        outputfield_too_short = 0
        OTHERS                = 0.
  ENDMETHOD.

  METHOD handle_link_click.
    DATA:
      lv_count       TYPE i,
      lv_tabix       TYPE i,
      lv_refresh_alv TYPE abap_bool.

    DATA:
      ls_stable TYPE lvc_s_stbl.

    DATA:
      lr_tadir TYPE REF TO zkco_samples_salv_tadir.

    FIELD-SYMBOLS:
      <ls_tadir_output>     TYPE zkco_samples_salv_tadir_output,
      <ls_tadir_output_new> TYPE zkco_samples_salv_tadir_output.

    CASE column.
      WHEN 'EXPAND'.
        READ TABLE gt_tadir_output ASSIGNING <ls_tadir_output> INDEX row.
        IF <ls_tadir_output>-expand(3) EQ icon_expand(3).
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).
          LOOP  AT gt_tadir REFERENCE INTO lr_tadir
                WHERE pgmid  EQ <ls_tadir_output>-pgmid
                  AND object EQ <ls_tadir_output>-object.
            lv_count = lv_count + 1.
            INSERT INITIAL LINE INTO gt_tadir_output INDEX row + lv_count ASSIGNING <ls_tadir_output_new>.
            MOVE-CORRESPONDING lr_tadir->* TO <ls_tadir_output_new>.
          ENDLOOP.
        ELSE.
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
          DELETE gt_tadir_output  FROM row + 1
                                  WHERE pgmid  EQ <ls_tadir_output>-pgmid
                                    AND object EQ <ls_tadir_output>-object.
        ENDIF.

        ls_stable-row = abap_true.
        ls_stable-col = abap_true.
        go_salv_table->refresh( s_stable     = ls_stable
                                refresh_mode = if_salv_c_refresh=>soft ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
