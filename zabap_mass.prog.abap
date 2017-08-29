REPORT zabap_mass.

DATA: gt_objects TYPE TABLE OF dwinactiv.

PARAMETERS: p_devc  TYPE devclass OBLIGATORY,
            p_cnt   TYPE i DEFAULT 2 OBLIGATORY,
            p_start TYPE i.

PARAMETERS: p_doma TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_clas TYPE c RADIOBUTTON GROUP g1.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  ASSERT p_cnt > 0.

  DO p_cnt TIMES.
    cl_progress_indicator=>progress_indicate(
        i_text               = 'Creating'
        i_processed          = sy-index
        i_total              = p_cnt
        i_output_immediately = abap_true ).

    CASE abap_true.
      WHEN p_doma.
        PERFORM create_domain USING sy-index.
      WHEN p_clas.
        PERFORM create_class USING sy-index.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.
    COMMIT WORK.
  ENDDO.

  PERFORM activate.
  PERFORM update_package_tree.

ENDFORM.

FORM activate.

  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
    EXPORTING
      activate_ddic_objects  = abap_true
      with_popup             = abap_true
    TABLES
      objects                = gt_objects
    EXCEPTIONS
      excecution_error       = 1
      cancelled              = 2
      insert_into_corr_error = 3
      OTHERS                 = 4.

ENDFORM.

FORM update_package_tree.

  DATA: lv_tree TYPE dirtree-tname.


* update package tree for SE80
  lv_tree = 'EU_' && p_devc.
  CALL FUNCTION 'WB_TREE_ACTUALIZE'
    EXPORTING
      tree_name              = lv_tree
      without_crossreference = abap_true
      with_tcode_index       = abap_true.

ENDFORM.

FORM create_domain USING p_counter TYPE i.

  DATA: lv_name   TYPE ddobjname,
        ls_dd01v  TYPE dd01v,
        lv_num    TYPE n LENGTH 5,
        lt_dd07v  TYPE TABLE OF dd07v,
        ls_object TYPE ddenqs.


  lv_num = p_counter + p_start.
  lv_name = |ZMASS{ lv_num }|.

  ls_object-objtype = 'DOMA'.
  ls_object-objname = lv_name.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object              = ls_object
      object_class        = 'DICT'
      devclass            = p_devc
      master_language     = sy-langu
      mode                = 'INSERT'
    EXCEPTIONS
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      OTHERS              = 4.
  ASSERT sy-subrc = 0.

  ls_dd01v-domname   = lv_name.
  ls_dd01v-datatype  = 'CHAR'.
  ls_dd01v-leng      = 10.
  ls_dd01v-outputlen = 10.

  CALL FUNCTION 'DDIF_DOMA_PUT'
    EXPORTING
      name              = lv_name
      dd01v_wa          = ls_dd01v
    TABLES
      dd07v_tab         = lt_dd07v
    EXCEPTIONS
      doma_not_found    = 1
      name_inconsistent = 2
      doma_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.
  ASSERT sy-subrc = 0.

  APPEND INITIAL LINE TO gt_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
  <ls_obj>-object = 'DOMA'.
  <ls_obj>-obj_name = lv_name.

ENDFORM.

FORM create_class USING p_counter TYPE i.

  DATA: lv_num       TYPE n LENGTH 5,
        ls_vseoclass TYPE vseoclass.


  lv_num = p_counter + p_start.

  ls_vseoclass-clsname   = |ZCL_MASS{ lv_num }|.
  ls_vseoclass-version   = '1'.
  ls_vseoclass-langu     = sy-langu.
  ls_vseoclass-descript  = 'foobar'.
  ls_vseoclass-exposure  = '2'.
  ls_vseoclass-state     = '1'.
  ls_vseoclass-clsfinal  = abap_true.
  ls_vseoclass-clsccincl = abap_true.
  ls_vseoclass-fixpt     = abap_true.
  ls_vseoclass-unicode   = abap_true.

  CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
    EXPORTING
      devclass        = p_devc
    CHANGING
      class           = ls_vseoclass
    EXCEPTIONS
      existing        = 1
      is_interface    = 2
      db_error        = 3
      component_error = 4
      no_access       = 5
      other           = 6
      OTHERS          = 7.
  ASSERT sy-subrc = 0.

* todo, activation of CLAS

ENDFORM.
