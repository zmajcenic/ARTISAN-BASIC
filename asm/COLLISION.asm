; ************************************************************************************************
; quick test if HL<=DE<=HL+BC 
; input BC=width, DE=x, HL=min
; if not true flag C set
GENERIC_INNER_CHECK:
    AND A 
    SBC HL, DE 
    JP P, GENERIC_INNER_CHECK_NOT
    AND A
    ADC HL, BC 
    JP M, GENERIC_INNER_CHECK_NOT
    AND A
    RET
GENERIC_INNER_CHECK_NOT:
    SCF 
    RET
; ************************************************************************************************
