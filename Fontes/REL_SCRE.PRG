*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_SCRE.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE SALDO CREDOR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_SCRE()
SAVE SCREEN TO TELA03
PTIT( "RELATàRIO DE SALDO CREDOR")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
GOTO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 3
USE CAD_QUAD INDEX CAD_QUAD
GOTO TOP
SELE 4
USE CAD_APAR INDEX CAD_APAR
GOTO TOP
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SELE 6
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 8
* Cadastro de Parcelas
SELE 9
USE MSG_BOLE INDEX MSG_PROD,MSG_BOLE
GOTO TOP
SELE 2
* Selecao do produto
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 05,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(5,15,5,70,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
eDATA_INI   = DATE()
eDATA_FIM   = CTOD("31/12/2999")
DO WHILE .T.
   @ 08,02 SAY "Periodo : " GET eDATA_INI
   @ 08,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv lidas !",3)
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
eCLAS_PRO = STR(eCLAS_PRO,5)
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"SALDOCREDOR.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "SALDOCREDOR.TXT"+SPACE(10) 
ENDIF    
eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja extrato em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      PLQUA("01")
	  PMENSAG("Insira um disco no drive (X:), e tecle ENTER.")
      PPARA()
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO
PLQUA("01")
PPERG ( "Deseja Efetuar a Consolida‡Æo do Arquivo de Saldo (S/N) ? ")
IF sisRESP = "S"
   eCONVERT = .F.
   PLQUA("01")
   PPERG ( "Converter Saldos Anteriores a 01/07/1994 em Real (R$) (S/N) ? ")
   IF sisRESP = "S"
      eCONVERT = .T.
   ENDIF
   eTOTAIS = .F.
   PLQUA("01")
   PPERG ( "Deseja apenas Totais (S/N) ? ")
   IF sisRESP = "S"
      eTOTAIS = .T.
   ENDIF
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados para ImpressÆo ...")
   IF eTIPO_PRO = 1
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 3
      SET FILTER TO NUM_LOTEAM = eCLAS_PRO .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = eCLAS_PRO
         eIND_QUAD = NUM_QUADRA
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         APPEND FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
         GOTO TOP
         SELE 3
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      IF eCONVERT
         GOTO TOP
         DO WHILE .NOT. EOF()
            IF DATA_PGTO <= CTOD("01/07/1994")
               IF F_BLOQR()
                  REPLACE VLR_PGTO WITH VLR_PGTO / 2750
                  UNLOCK
               ENDIF
            ENDIF
            SKIP
         ENDDO
      ENDIF
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      SET EXCLUSIVE OFF
   ELSE
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 4
      SET FILTER TO NUM_LOTEAM = eCLAS_PRO .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = eCLAS_PRO
         eIND_QUAD = NUM_BLOCO
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         APPEND FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
         GOTO TOP
         SELE 4
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      IF eCONVERT
         GOTO TOP
         DO WHILE .NOT. EOF()
            IF DATA_PGTO <= CTOD("01/07/1994")
               IF F_BLOQR()
                  REPLACE VLR_PGTO WITH VLR_PGTO / 2750
                  UNLOCK
               ENDIF
            ENDIF
            SKIP
         ENDDO
      ENDIF
      GOTO TOP
      SET EXCLUSIVE OFF
   ENDIF
ENDIF
SELE 6
SET FILTER TO
GOTO TOP
SELE 8
IF F_BLOQA()
   PACK
   UNLOCK
ENDIF
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Pagamentos Neste Per¡odo !")
   PPARA()
ELSE
   IF eSAIDA = "ARQUIVO"
      SET PRINT ON
      SET DEVICE TO PRINT
      SET PRINTER TO &eARQUIVO
   ELSE
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      SET DEVICE TO PRINT
   ENDIF
   eSUB_SD = 0
   eSUB_PA = 0
   eTOT_SD = 0
   eTOT_PA = 0
   eCONTRATO = 0
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN004"
      @ 03,20  SAY sisLETRAE+"RELATORIO SALDO CREDOR DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      @ 04,00  SAY "Produto : "+eCLAS_PRO+" - "+ALLTRIM(eDESC_PRO)
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,02  SAY "No. IMOVEL"
      @ 06,15  SAY "NOME CLIENTE"
      @ 06,70  SAY "SD. CREDOR"
      @ 06,95  SAY "PARCELAS"
      @ 06,115 SAY "CONTRATOS"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         IF eTIPO_PRO = 1
            eCLAS_PRO   = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
         ELSE
            eCLAS_PRO   = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
         ENDIF
         SELE 6
         SEEK eCLAS_PRO+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         eNUM_CLIE = NUM_CLIE
         SELE 5
         SEEK eNUM_CLIE
         eCLIENTE = CLIENTE
         SELE 8
         eSUB_SD = 0
         eSUB_PA = 0
         DO WHILE (CLASSE = eCLAS_PRO .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE) .AND. (.NOT. EOF())
            IF DATA_VENC < CTOD("01/07/1994") .AND. eCONVERT
               eSUB_SD = eSUB_SD + (VLR_PARC / 2750)
            ELSE
               eSUB_SD = eSUB_SD + VLR_PARC
            ENDIF
            eSUB_PA = eSUB_PA + 1
            SKIP
         ENDDO
         IF .NOT. eTOTAIS
            @ LIN,02  SAY STR(ePARTE,1)+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)
            @ LIN,15  SAY eCLIENTE
            @ LIN,70 SAY eSUB_SD PICT "@R 999,999,999,999.99"
            @ LIN,95 SAY eSUB_PA PICT "@R 999,999"
            LIN = LIN + 1
            eCONTRATO = eCONTRATO + 1
         ENDIF
         eTOT_SD = eTOT_SD + eSUB_SD
         eTOT_PA = eTOT_PA + eSUB_PA
         DO PTECLA
         IF sisINTERR
            IF eSAIDA = "ARQUIVO"
               SET PRINT TO
               SET PRINT OFF
               SET DEVICE TO SCREEN
            ELSE
               SET PRINT TO
               SET DEVICE TO SCREEN
            ENDIF
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
      IF EOF()
         LIN = LIN + 1
         @ LIN,02  SAY "Total Geral    : "
         @ LIN,70  SAY eTOT_SD   PICT "@R 999,999,999,999.99"
         @ LIN,95  SAY eTOT_PA   PICT "@R 999,999"
         @ LIN,115 SAY eCONTRATO PICT "@R 999"
      ENDIF
      SELE 8
      @ 56,00 SAY REPLICATE("=",135)
      @ 57,104 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
ENDIF
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
