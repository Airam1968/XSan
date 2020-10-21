*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RNG.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE ROTINAS ESPECIAIS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RNG()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU DE ROTINAS ESPECIAIS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 20,70
   @ 11,50 TO 20,70
   @ 12,51 PROMPT " 1     Correá∆o    " MESSAGE " Rotina Especial de Correá∆o Monet†ria           "
   @ 13,51 PROMPT " 2     Quitaá∆o    " MESSAGE " Rotina Especial de Quitaá∆o do Financiamento    "
   @ 14,51 PROMPT " 3     Permutas    " MESSAGE " Rotina Especial de Permuta                      "
   @ 15,51 PROMPT " 4   Renegociaá∆o  " MESSAGE " Rotina Especial de Renegociaá∆o do Financiamento"
   @ 16,51 PROMPT " 5    Desistància  " MESSAGE " Rotina Especial de Desistància do Financiamento "
   @ 17,51 PROMPT " 6   Propriet†rio  " MESSAGE " Rotina Especial de Transferància de Propriet†rio"
   @ 18,51 PROMPT " 7 Troca Indexador " MESSAGE " Rotina Especial de Mudanáa de Indexador         "   
   @ 19,51 PROMPT " Esc    Fim        " MESSAGE " Fim de Operaá∆o do M¢dulo                       "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_AL_03")
           CORRECAO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ROTINA DE CORRECAO MENSAL","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_AL_03")
           QUI_IMOV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ROTINA DE QUITACAO","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_AL_03")
           PER_IMOV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ROTINA DE PERMUTA DE IMOVEL","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_AL_03")
           RNG_IMOV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ROTINA DE RENEGOCIACAO","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_AL_03")
           DES_IMOV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ROTINA DE DESISTENCIA","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_AL_03")
           PRO_IMOV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ROTINA DE PERMUTA","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_AL_03")
           IND_IMOV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ROTINA DE TROCA DE INDEXADOR","ACESSO NEGADO")                                
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************

