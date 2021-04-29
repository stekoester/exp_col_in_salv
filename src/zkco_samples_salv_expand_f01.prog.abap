FORM data_get.
  DATA:
    ls_tadir_output TYPE zkco_samples_salv_tadir_output,
    ls_tadir        TYPE zkco_samples_salv_tadir,
    ls_cell_type    TYPE salv_s_int4_column.

  FIELD-SYMBOLS:
    <lv_object> TYPE trobjtype.

  ls_cell_type-columnname = 'EXPAND'.
  ls_cell_type-value      = if_salv_c_cell_type=>hotspot.

  APPEND ls_cell_type TO ls_tadir_output-cell_type.
  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE gcs_tadir_objects TO <lv_object>.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    SELECT pgmid object obj_name korrnum srcsystem author srcdep devclass
      FROM tadir APPENDING TABLE gt_tadir               "#EC CI_GENBUFF
      UP TO 5 ROWS
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ <lv_object>
        AND devclass EQ 'SCTS_CAT'.
    IF sy-subrc EQ 0.
      READ TABLE gt_tadir INTO ls_tadir INDEX lines( gt_tadir ).
      ls_tadir_output-pgmid     = ls_tadir-pgmid.
      ls_tadir_output-object    = ls_tadir-object.
      ls_tadir_output-expand    = lcl_handle_events=>get_icon( iv_type = 'E' ).
      APPEND ls_tadir_output TO gt_tadir_output.
    ENDIF.
  ENDDO.
ENDFORM.

FORM data_display.
  CALL SCREEN 100.
ENDFORM.

FORM alv_display.
  DATA:
    lv_icon             TYPE string.

  DATA:
    lo_custom_container TYPE REF TO cl_gui_custom_container,
    lr_events           TYPE REF TO cl_salv_events_table,
    lr_event_handler    TYPE REF TO lcl_handle_events.

  TRY.
      CREATE OBJECT lo_custom_container
        EXPORTING
          container_name = 'ALV'.
      IF sy-subrc EQ 0.
      ENDIF.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = lo_custom_container
        IMPORTING
          r_salv_table   = go_salv_table
        CHANGING
          t_table        = gt_tadir_output ).

      lr_events = go_salv_table->get_event( ).
      CREATE OBJECT lr_event_handler.
      SET HANDLER lr_event_handler->handle_added_function FOR lr_events.
      SET HANDLER lr_event_handler->handle_link_click FOR lr_events.

      TRY.
          go_salv_table->get_columns( )->set_cell_type_column( 'CELL_TYPE' ).
        CATCH cx_salv_data_error.
      ENDTRY.
      TRY.
          go_salv_table->get_columns( )->set_optimize( ).
        CATCH cx_salv_data_error.
      ENDTRY.

      TRY.
          lv_icon = lcl_handle_events=>gcs_toolbar-expall_icon.
          go_salv_table->get_functions( )->add_function( name = lcl_handle_events=>gcs_toolbar-expall_name
                                                         tooltip = lcl_handle_events=>gcs_toolbar-expall_tooltip
                                                         icon = lv_icon
                                                         position = lcl_handle_events=>gcs_toolbar-expall_position ).
        CATCH cx_salv_wrong_call.
        CATCH cx_salv_existing.
      ENDTRY.
      TRY.
          lv_icon = lcl_handle_events=>gcs_toolbar-colall_icon.
          go_salv_table->get_functions( )->add_function( name = lcl_handle_events=>gcs_toolbar-colall_name
                                                         tooltip = lcl_handle_events=>gcs_toolbar-colall_tooltip
                                                         icon = lv_icon
                                                         position = lcl_handle_events=>gcs_toolbar-colall_position ).
        CATCH cx_salv_wrong_call.
        CATCH cx_salv_existing.
      ENDTRY.

      TRY.
          go_salv_table->get_columns( )->get_column( 'EXPAND' )->set_alignment( if_salv_c_alignment=>centered ).
        CATCH cx_salv_not_found.
      ENDTRY.
      go_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).

      go_salv_table->display( ).
    CATCH cx_salv_msg.
  ENDTRY.
ENDFORM.
