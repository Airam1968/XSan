*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_PROS.PRG
* Data        : 01/01/99
* Objetivo    : Exportaá∆o de dados para contabilidade - Prosoft
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXP_PROS()
SAVE SCREEN TO TELA05
PTIT( "EXPORTAÄ«O PARA CONTABILIDADE - PROSOFT")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA05
   RETURN nil
ENDIF
SELE 7
USE BANCOS INDEX BANCOS
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 5
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA <> 0
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
         PMENSAG("Conta N∆o Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eCOD_BANCO  = COD_BANCO
eNOME_CONTA = ALLTRIM(COD_AGENCI)+" - "+ALLTRIM(COD_CC)
eLIMITE     = LIMITE
SELE 7
SEEK eCOD_BANCO
eNOME_BANCO = ALLTRIM(NOME_BANCO)
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv†lidas !",3)
      LOOP
   ELSE
      IF eDATA_INI > eDATA_FIM
         PLQUA("01")
         PMSGR( "Data Inicio > Que Final !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
ENDDO
PLQUA("01")
PPERG ( "Deseja Gerar Arquivo (S/N) ? ")
IF sisRESP = "N"
   CLOSE ALL
   RESTORE SCREEN FROM TELA05
   RETURN nil
ELSE
   SET EXCLUSIVE ON
   SELE 6
   eCONT  = STR(eCONTA,5)
   eZERO = "00"
   eARQ  = "EXP_PROS"
   eIND1 = "EXP_PROS"
   USE &eARQ INDEX &eIND1
   GOTO TOP
   eDATA_ORI = DATA
   DELE ALL
   PACK
   GOTO TOP
   SET EXCLUSIVE OFF
   SELE 6
   USE
   USE &eARQ INDEX &eIND1
   GOTO TOP
   SELE 1
   USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE <> CTOD("")
   GOTO TOP
   SELE 3
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA
   GOTO TOP
   eDATA_AUX = eDATA_INI
   eSALDO_I  = 0
   eAPLIC_D  = 0
   eAPLIC_B  = 0
****
**** PAREI AQUI
****
   DO WHILE .T.
      SELE 6
      SEEK eDATA_AUX
      IF FOUND()
         DO WHILE DATA = eDATA_AUX
            SKIP
         ENDDO
         SKIP -1
         IF SALDO_F = 0
            eSALDO_I = SALDO_I
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ELSE
            eSALDO_I = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         EXIT
      ELSE
         eDATA_AUX = eDATA_AUX - 1
         LOOP
      ENDIF
   ENDDO
   eDATA_AUX = eDATA_INI
   SELE 6
   APPEND BLANK
   IF F_BLOQR()
      REPLACE DATA       WITH eDATA_AUX,;
              CONTA      WITH eCONTA,;
              DESCRICAO  WITH "SALDO INICIAL "
      REPLACE SALDO_I    WITH eSALDO_I,;
              ENTRADAS   WITH 0,;
              SAIDAS     WITH 0
      REPLACE SALDO_F    WITH eSALDO_I,;
              APLICACAOD WITH eAPLIC_D,;
              APLICACAOB WITH eAPLIC_B
      UNLOCK
      eTRANSP  = eSALDO_I
      eAPLIC_D = APLICACAOD
      eAPLIC_B = APLICACAOB
   ENDIF
   DO WHILE eDATA_AUX <= eDATA_FIM
      PLQUA("01")
	  PMENSAG("Aguarde o Processamento, Dia "+DTOC(eDATA_AUX))
      SELE 3
      GOTO TOP
      SET FILTER TO DATA_REAL = eDATA_AUX .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA
      GOTO TOP
      DO WHILE .NOT. EOF()
         eCOMPENSADO = SPACE(1)
         eVLR_REAL  = 0
         eCLASSE_R  = 0
         eDESCRICAO = SPACE(40)
         eVLR_REAL  = REALIZADO
         eCLASSE_R  = CLASSE_R
         eDESCRICA2 = DESCRICAO
         IF eCLASSE_R < 0
            IF eCLASSE_R = -1
               eAPLIC_D = eAPLIC_D - eVLR_REAL
            ENDIF
            IF eCLASSE_R = -2
               eAPLIC_B = eAPLIC_B - eVLR_REAL
            ENDIF
         ENDIF
         IF DATA_COMPE <> CTOD("")
            eCOMPENSADO = "*"
         ENDIF
         SELE 5
         SEEK eCLASSE_R
         eDESCRICAO = DESCRICAO
         SELE 6
         APPEND BLANK
         IF F_BLOQR()
            REPLACE DATA       WITH eDATA_AUX,;
                    CONTA      WITH eCONTA,;
                    DESCRICAO  WITH eDESCRICAO
            REPLACE SALDO_I    WITH eTRANSP,;
                    ENTRADAS   WITH eVLR_REAL,;
                    SAIDAS     WITH 0
            REPLACE SALDO_F    WITH (SALDO_I+ENTRADAS)-SAIDAS,;
                    APLICACAOD WITH eAPLIC_D,;
                    APLICACAOB WITH eAPLIC_B
            REPLACE DESCRICA2  WITH eDESCRICA2,;
                    COMPENSADO WITH eCOMPENSADO
            UNLOCK
            eTRANSP = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         SELE 3
         SKIP
      ENDDO
      SELE 1
      GOTO TOP
      SET FILTER TO DATA_REAL = eDATA_AUX .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA
      GOTO TOP
      DO WHILE .NOT. EOF()
         eCOMPENSADO = SPACE(1)
         eVLR_REAL   = 0
         eCLASSE_D   = 0
         eDESCRICAO  = SPACE(40)
         eNUM_CHEQUE = 0
         eVLR_REAL   = REALIZADO
         eCLASSE_D   = CLASSE_D
         eNUM_CHEQUE = NUM_CHEQUE
         eDESCRICA2  = DESCRICAO
         IF eCLASSE_D < 0
            IF eCLASSE_D = -1
               eAPLIC_D = eAPLIC_D + eVLR_REAL
            ENDIF
            IF eCLASSE_D = -2
               eAPLIC_B = eAPLIC_B + eVLR_REAL
            ENDIF
         ENDIF
         IF DATA_COMPE <> CTOD("")
            eCOMPENSADO = "*"
         ENDIF
         SELE 4
         SEEK eCLASSE_D
         eDESCRICAO = DESCRICAO
         SELE 6
         APPEND BLANK
         IF F_BLOQR()
            REPLACE DATA       WITH eDATA_AUX,;
                    CONTA      WITH eCONTA,;
                    DESCRICAO  WITH eDESCRICAO
            REPLACE SALDO_I    WITH eTRANSP,;
                    ENTRADAS   WITH 0,;
                    SAIDAS     WITH eVLR_REAL
            REPLACE SALDO_F    WITH (SALDO_I+ENTRADAS)-SAIDAS,;
                    NUM_CHEQUE WITH eNUM_CHEQUE
            REPLACE APLICACAOD WITH eAPLIC_D,;
                    APLICACAOB WITH eAPLIC_B,;
                    DESCRICA2  WITH eDESCRICA2
            REPLACE COMPENSADO WITH eCOMPENSADO
            UNLOCK
            eTRANSP = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         SELE 1
         SKIP
      ENDDO
      eDATA_AUX = eDATA_AUX + 1
   ENDDO
ENDIF
SELE 6
GOTO TOP
SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
GOTO TOP
PLIMPA()
@ 05,00 SAY eNOME_BANCO+" - "+eNOME_CONTA
SET COLOR TO W/N,+W/R
DECLARE CAMPO[11],PIC[11],CAB[11],LENS[11]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="X"
PIC[2] ="@R 999"
PIC[3] ="  /  /    "
PIC[4] ="@S28"
PIC[5] ="@R 999,999,999.99"
PIC[6] ="@R 999,999,999.99"
PIC[7] ="@R 999,999,999.99"
PIC[8] ="@R 999,999,999.99"
PIC[9] ="@R 999,999,999"
PIC[10]="@R 999,999,999.99"
PIC[11]="@R 999,999,999.99"
CAB[1] ="*"
CAB[2] ="CONTA"
CAB[3] ="DATA"
CAB[4] ="DESCRICAO"
CAB[5] ="INICIAL (+)"
CAB[6] ="ENTRADAS (+)"
CAB[7] ="SAIDAS (-)"
CAB[8] ="FINAL (=)"
CAB[9] ="NUM. CHEQUE"
CAB[10]="SALDO APLIC. DESBLOQ."
CAB[11]="SALDO APLIC. BLOQUEIO"
@ 6,00 TO 21,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ESC]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,06 SAY "Para Sair"
DBEDIT(07,01,21,78,CAMPO,"F_SALD",PIC,CAB,CHR(196),CHR(179),CHR(205))
eOPCAO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Deseja Cheque/Depositos (T)otais ou do (P)eráodo selecionado (T/P) ? " GET eOPCAO PICT "@! X" VALID eOPCAO $("TP")
READ
PLIMPA()
GOTO BOTTOM                                          
@ 08,15 SAY "Saldo Final          : " GET SALDO_F     PICT "@R 999,999,999,999.99"
eSALDO_L = SALDO_F + eLIMITE
@ 10,15 SAY "Saldo c\Limite Cred. : " GET eSALDO_L    PICT "@R 999,999,999,999.99"
@ 12,15 SAY "Aplicaááes Desbloque.: " GET APLICACAOD  PICT "@R 999,999,999,999.99"
@ 14,15 SAY "Aplicaááes Bloqueadas: " GET APLICACAOB  PICT "@R 999,999,999,999.99"
eGERAL = 0
eGERAL = SALDO_F + APLICACAOD + APLICACAOB + eLIMITE
@ 16,15 SAY "Saldo Final Geral    : " GET eGERAL      PICT "@R 999,999,999,999.99"
SELE 1
USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG
GOTO TOP
IF eOPCAO = "T"
   SET FILTER TO REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("") .AND. NUM_CHEQUE <> 0
   GOTO TOP
ELSE
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND.REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("") .AND. NUM_CHEQUE <> 0
   GOTO TOP
ENDIF
eCHE_COMP = 0
DO WHILE .NOT. EOF()
   eCHE_COMP = eCHE_COMP + REALIZADO
   SKIP
ENDDO
@ 18,15 SAY "Cheques a Compensar  : " GET eCHE_COMP   PICT "@R 999,999,999,999.99"
SELE 1
USE CRECEBER INDEX DATA_CRE,CRECEBER
GOTO TOP
IF eOPCAO = "T"
   SET FILTER TO REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("")
   GOTO TOP
ELSE
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND.REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("")
   GOTO TOP
ENDIF
eDEP_COMP = 0
DO WHILE .NOT. EOF()
   eDEP_COMP = eDEP_COMP + REALIZADO
   SKIP
ENDDO
@ 20,15 SAY "Deposito a Compensar : " GET eDEP_COMP   PICT "@R 999,999,999,999.99"
CLEAR GETS
PPARA()
CLOSE ALL
RESTORE SCREEN FROM TELA05
RETURN nil
**************************  fim  *********************************
