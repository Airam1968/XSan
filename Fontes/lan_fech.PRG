*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : LAN_FECH.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA A SITUACAO DE COTACOES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION LAN_FECH()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA A SITUACAO DE COTACOES")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 2
   USE COTACAO INDEX COTACAO
   GOTO TOP
   SET FILTER TO SITUACAO = "PEDIDO DE COMPRA"
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[10],PIC[10],CAB[10],LENS[10]
   CAMPO[1] ="NUMERO"
   CAMPO[2] ="FORNECED_1"
   CAMPO[3] ="FORNECED_2"
   CAMPO[4] ="FORNECED_3"
   CAMPO[5] ="FORNECED_4"
   CAMPO[6] ="FORNECED_5"
   CAMPO[7] ="SITUACAO"
   CAMPO[8] ="MENOR"
   CAMPO[9] ="TOTAL"
   CAMPO[10] ="LANCAMENTO"
   LENS[1] =10
   LENS[2] =10
   LENS[3] =10
   LENS[4] =10
   LENS[5] =10
   LENS[6] =10
   LENS[7] =10
   LENS[8] =10
   LENS[9] =19
   LENS[10]=10
   PIC[1] ="@R 9999999"
   PIC[2] ="@R 9999999"
   PIC[3] ="@R 9999999"
   PIC[4] ="@R 9999999"
   PIC[5] ="@R 9999999"
   PIC[6] ="@R 9999999"
   PIC[7] ="@S20"
   PIC[8] ="@R 99999999"
   PIC[9] ="@R 999,999,999,999.99"
   PIC[10] ="@R 99999999"
   CAB[1] ="COTAá∆o"
   CAB[2] ="FORN 1"
   CAB[3] ="FORN 2"
   CAB[4] ="FORN 3"
   CAB[5] ="FORN 4"
   CAB[6] ="FORN 5"
   CAB[7] ="SITUAá∆o"
   CAB[8] ="MENOR"
   CAB[9] ="TOTAL" 
   CAB[10] ="LANC. CP"
   @ 5,00 TO 21,79 DOUBLE
   PLQUA("01")
   SET COLOR TO
   SET COLOR TO W/N+*
   @ 23,00 SAY "[ESC]"
   SET COLOR TO
   SET COLOR TO W/N,+W/R
   @ 23,07 SAY "Para Sair"
   DBEDIT(06,01,21,78,CAMPO,"F_CON_FECH",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_CON_FECH
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         PLQUA("01")
         PPERG ( "Deseja Efetuar o Fechamento da Compra (S/N) ? ")
         IF sisRESP = "S"
            eMENOR   = MENOR
            eCOTACAO = NUMERO
            eTOTAL   = TOTAL
            PLQUA("01")
            SAVE SCREEN TO TELA04
            PLIMPA()
			PTIT( "CADASTRO DE CONTAS A PAGAR")
            SELE 5
            USE CLAS_PRO INDEX CLAS_PRO
            GOTO TOP
            SELE 4
            USE CLASSE_D INDEX CLASSE_D
            GOTO TOP
            SELE 3
            USE STATUS
            GOTO TOP
            SELE 8
            USE FORNECED INDEX FORNECED
            GOTO TOP
            SELE 1
            USE CPAGAR INDEX CPAGAR,DATA_CPG,DATA_CPV,NUM_CHEQ
            GOTO TOP
            PLQUA("01")
            PPERG ( "Confirma (S/N) ? ")
            IF sisRESP = "S"
               DO WHILE .T.
                  PLQUA("11")
                  eNUM_LANC   = 0
                  eCLASSE_D   = 0
                  eCLASSE_PRO = 0
                  eCODIGO_FOR = eMENOR
                  eDESCRICAO  = SPACE(50)   
                  eDATA_PREV  = CTOD("")
                  ePREVISTO   = eTOTAL
                  eDATA_REAL  = CTOD("")   
                  eREALIZADO  = 0
                  eCOD_CONTA  = 0
                  eNUM_CHEQ   = 0
                  eNUM_DOCUME = "OC No."+STR(eCOTACAO,5)
                  SELE 3
                  IF F_BLOQR()
                     eNUM_LANC  = LANC_CPG
                     eORDE_FORN = ORDE_FORN
                     eORDE_CLAD = ORDE_CLAD
                     REPLACE LANC_CPG WITH eNUM_LANC + 1
                     UNLOCK
                  ENDIF
                  IF eORDE_FORN
                     SELE 8
                     USE FORNECED
                     INDEX ON NOME TO TEMP_FOR
                     GOTO TOP
                  ENDIF
                  IF eORDE_CLAD
                     SELE 4
                     USE CLASSE_D
                     INDEX ON DESCRICAO TO TEMP_CLD
                     GOTO TOP
                  ENDIF
                  eNUM_ANTE = eNUM_LANC
                  @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
                  READ
                  IF eNUM_LANC = 0
                     SELE 3
                     IF F_BLOQR()
                        REPLACE LANC_CPG WITH eNUM_ANTE
                        UNLOCK
                     ENDIF
                     CLOSE ALL
                     RESTORE SCREEN FROM TELA04
                     RETURN nil
                  ENDIF
                  SELE 1
                  GOTO TOP
                  SEEK eNUM_LANC
                  IF FOUND()
                     PLQUA("01")
                     PMSGR( "Atená∆o !",3)
					 PMENSAG("Lanáamento Já Existe")
                     PPARA()
                     LOOP
                  ENDIF
                  SELE 4
                  GOTO TOP
                  IF .NOT. eORDE_CLAD
                     DO WHILE .T.   
                        eCLASSE_D = 0
                        @ 08,05 SAY "Cod. Despesa  : " GET eCLASSE_D PICT "@R 99999999"
                        READ
                        IF eCLASSE_D = 0
                           SET COLOR TO W/N,+W/R
                           eCLASSE_D = 0
                           @ 08,00
                           @ 08,05 SAY "Cod. Despesa  : "
                           SELE 4
                           GO TOP
                           DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
                           AFIELDS(CAMPO,.T.,LENS)
                           PIC[1]="@R99999"
                           PIC[2]="@S40"
                           CAB[1]="    "
                           CAB[2]="    "
                           DBEDIT(8,20,8,75,CAMPO,"FCON_CTPG",PIC,CAB," "," - "," ")
                           eCLASSE_D = CODIGO
                           EXIT
                        ELSE
                           SEEK eCLASSE_D
                           IF FOUND()
                              @ 08,31 SAY " - "+ALLTRIM(DESCRICAO)
                              SELE 1
                              EXIT
                           ELSE
                              PLQUA("01")
                              PMSGR( "Classe Inexistente !",3)
                              LOOP
                           ENDIF
                        ENDIF
                     ENDDO
                  ELSE
                     DO WHILE .T.   
                        eCLASSE_D = SPACE(30)
                        @ 08,05 SAY "Cod. Despesa  : " GET eCLASSE_D PICT "@S30"
                        READ
                        eCLASSE_D = ALLTRIM(eCLASSE_D)
                        SELE 4
                        SEEK eCLASSE_D
                        SET COLOR TO W/N,+W/R
                        @ 08,00
                        @ 08,05 SAY "Cod. Despesa  : "
                        DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
                        AFIELDS(CAMPO,.T.,LENS)
                        PIC[1]="@R99999"
                        PIC[2]="@S40"
                        CAB[1]="    "
                        CAB[2]="    "
                        DBEDIT(8,20,8,75,CAMPO,"FCON_CTPG",PIC,CAB," "," - "," ")
                        eCLASSE_D = CODIGO
                        EXIT
                     ENDDO
                  ENDIF
                  SELE 5
                  GOTO TOP
                  DO WHILE .T.
                     @ 09,05 SAY "Cod. Empreend.: " GET eCLASSE_PRO PICT "@R 99999999"
                     READ
                     IF eCLASSE_PRO = 0
                        EXIT
                     ELSE
                        SEEK eCLASSE_PRO
                        IF FOUND()
                           @ 09,31 SAY " - "+ALLTRIM(DESCRICAO)
                           SELE 1
                           EXIT
                        ELSE
                           PLQUA("01")
                           PMSGR( "Empreendimento Inexistente !",3)
                           LOOP
                        ENDIF
                     ENDIF
                  ENDDO
                  IF .NOT. eORDE_FORN
                     DO WHILE .T.
                        @ 10,05 SAY "Fornecedor    : " GET eCODIGO_FOR PICT "@R 9999999"
                        READ
                        SELE 8
                        SEEK eCODIGO_FOR
                        SET COLOR TO W/N,+W/R
                        eCODIGO_FOR = 0
                        @ 10,00
                        @ 10,05 SAY "Fornecedor    : "
                        SELE 8
                        DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
                        AFIELDS(CAMPO,.T.,LENS)
                        PIC[1]="@R99999"
                        PIC[2]="@S40"
                        CAB[1]="    "
                        CAB[2]="    "
                        DBEDIT(10,20,10,75,CAMPO,"FCON_CTPG",PIC,CAB," "," - "," ")
                        eCODIGO_FOR = CODIGO
                        EXIT
                     ENDDO
                  ENDIF
                  SELE 1
                  DO WHILE .T.
                     @ 11,05 SAY "Data Prevista : " GET eDATA_PREV
                     READ
                     IF eDATA_PREV = CTOD("")
                        LOOP
                     ELSE
                        EXIT
                     ENDIF
                  ENDDO
                  @ 12,05 SAY "Vlr. Previsto : " GET ePREVISTO  PICT "@R 999,999,999,999.99"
                  READ
                  @ 13,05 SAY "Num. Documento: " GET eNUM_DOCUME
                  READ
                  @ 14,05 SAY "Descriá∆o     : " GET eDESCRICAO
                  READ
                  @ 15,05 SAY "Data Pagamento: " GET eDATA_REAL
                  READ
                  @ 16,05 SAY "Vlr. Realizado: " GET eREALIZADO PICT "@R 999,999,999,999.99"
                  READ
                  @ 17,05 SAY "C¢digo Conta  : " GET eCOD_CONTA PICT "@R 999999999"
                  READ
                  @ 18,05 SAY "Num. Cheque   : " GET eNUM_CHEQ  PICT "@R 999999999"
                  READ
                  SELE 1
                  PLQUA("01")
                  PPERG ( "Confirma (S/N) ? ")
                  IF sisRESP = "S"
                     PLQUA("01")
					 PMENSAG("Gravando, Aguarde ...")
                     SELE 1
                     APPEND BLANK
                     IF F_BLOQR()
                        REPLACE LANCAMENTO WITH eNUM_LANC,;
                                CLASSE_D   WITH eCLASSE_D
                        REPLACE EMPREENDIM WITH eCLASSE_PRO,;
                                CODIGO_FOR WITH eCODIGO_FOR
                        REPLACE DESCRICAO  WITH eDESCRICAO,;
                                DATA_PREV  WITH eDATA_PREV,;
                                PREVISTO   WITH ePREVISTO
                        REPLACE DATA_REAL  WITH eDATA_REAL,;
                                REALIZADO  WITH eREALIZADO,;
                                COD_CONTA  WITH eCOD_CONTA
                        REPLACE NUM_CHEQUE WITH eNUM_CHEQ,;
                                NUM_DOCUME WITH eNUM_DOCUME,;
                                CONTABIL   WITH .T.
                        UNLOCK
                     ENDIF
                     F_LACESSO(sisUSUARIO,"CONTAS A PAGAR","INCLUSAO LANC. : "+ALLTRIM(STR(eNUM_LANC,5)))
                     SELE 2
                     IF F_BLOQR()
                        REPLACE LANCAMENTO WITH eNUM_LANC,;
                                SITUACAO   WITH "FECHADO"
                        UNLOCK
                     ENDIF
                     EXIT
                  ELSE
                     SELE 2
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
            RESTORE SCREEN FROM TELA04
         ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
