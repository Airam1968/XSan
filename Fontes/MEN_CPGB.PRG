*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MEN_CPGB.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO CONSULTA DE SITUACAO DO PRODUTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MEN_CPGB()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU CONSULTA DE SITUACAO DO PRODUTO")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 17,70
   @ 11,50 TO 17,70
   @ 12,51 PROMPT " 1   Proprietario  " MESSAGE " Consulta de Unidades por Proprietario        "
   @ 13,51 PROMPT " 2   Vend./Disp.   " MESSAGE " Consulta de Unidades Vendidas/Disponiveis    "
   @ 14,51 PROMPT " 3    Quitados     " MESSAGE " Consulta de Unidades Quitadas                "
   @ 15,51 PROMPT " 4   Em Processo   " MESSAGE " Consulta de Unidades em Processo Judicial    "
   @ 16,51 PROMPT " 5  Fim do M¢dulo  " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("CONSULTAS")
           DO CON_PRPR
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("CONSULTAS")
           DO CON_PROV
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("CONSULTAS")
           DO CON_PROQ
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("CONSULTAS")
           DO CON_PROP
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE sisOP = 5
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************
