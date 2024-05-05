*&---------------------------------------------------------------------*
*& Report z_demo_if_validation_params
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_demo_if_validation_params.

SELECT-OPTIONS: s_fecha FOR sy-datum NO-EXTENSION OBLIGATORY.

"Primera opción
CLASS lcl_validacion_fecha DEFINITION.
  PUBLIC SECTION.
    TYPES: t_fechas TYPE RANGE OF sy-datum.

    METHODS:
      fecha_no_mayor_a
        IMPORTING
          p_limite_dias     TYPE i
          p_fecha_a_validar TYPE t_fechas,

      fecha_anio_actual.

ENDCLASS.

CLASS lcl_validacion_fecha IMPLEMENTATION.

  METHOD fecha_anio_actual.

  ENDMETHOD.

  METHOD fecha_no_mayor_a.

  ENDMETHOD.

ENDCLASS.


"Segunda opción

INTERFACE lif_validation.
  TYPES: t_range_date TYPE RANGE OF sy-datum.

  METHODS:
    execute
      IMPORTING
        p_input TYPE t_range_date.

ENDINTERFACE.

CLASS lcl_validacion_fecha_if DEFINITION.
  PUBLIC SECTION.

    INTERFACES: lif_validation.

ENDCLASS.


CLASS lcl_validacion_fecha_if IMPLEMENTATION.

  METHOD lif_validation~execute.

    CONSTANTS: lv_dias_max TYPE i VALUE 30.

    READ TABLE p_input INTO DATA(ls_fecha) INDEX 1.

    IF sy-subrc EQ 0.

      IF ls_fecha-high - ls_fecha-low > lv_dias_max.
        WRITE: 'Se exedió el límite permitido IF'.
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_validacion_anio_if DEFINITION.
  PUBLIC SECTION.

    INTERFACES: lif_validation.

ENDCLASS.

CLASS lcl_validacion_anio_if IMPLEMENTATION.

  METHOD lif_validation~execute.

    READ TABLE p_input INTO DATA(ls_fecha) INDEX 1.

    IF sy-subrc EQ 0.

      IF s_fecha-high(4) NE sy-datum(4).
        WRITE: 'Año no permitido IF'.
      ENDIF.

      IF s_fecha-low(4) NE sy-datum(4).
        WRITE: 'Año no permitido IF'.
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION. "Evento principal

  IF s_fecha-high IS INITIAL.
    s_fecha-high = s_fecha-low.
  ENDIF.

  IF s_fecha-high - s_fecha-low > 30.
    WRITE: 'Se exedió el límite permitido'.
  ENDIF.

  IF s_fecha-high(4) NE sy-datum(4).
    WRITE: 'Año no permitido'.
  ENDIF.

  IF s_fecha-low(4) NE sy-datum(4).
    WRITE: 'Año no permitido'.
  ENDIF.

  DATA: lr_ref TYPE REF TO lif_validation.
  DATA: lv_clase  TYPE string,
        lt_clases TYPE TABLE OF string.

  APPEND 'LCL_VALIDACION_FECHA_IF' TO lt_clases.
  APPEND 'LCL_VALIDACION_ANIO_IF' TO lt_clases.

  LOOP AT lt_clases INTO lv_clase.
    CREATE OBJECT lr_ref TYPE (lv_clase).
    lr_ref = NEW lcl_validacion_fecha_if(  ).
    lr_ref->execute( p_input = s_fecha[] ).
  ENDLOOP.

*  lv_clase = 'LCL_VALIDACION_FECHA_IF'.

*  CREATE OBJECT lr_ref TYPE (lv_clase).
*  lr_ref = NEW lcl_validacion_fecha_if(  ).
*  lr_ref->execute( p_input = s_fecha[] ).

*  lv_clase = 'LCL_VALIDACION_ANIO_IF'.
*  CREATE OBJECT lr_ref TYPE (lv_clase).
*  lr_ref = NEW lcl_validacion_anio_if( ).
*  lr_ref->execute( p_input = s_fecha[] ).

*  DATA(lr_ref) = NEW lcl_validacion_fecha_if( )
