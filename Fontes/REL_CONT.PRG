*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CONT.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTABILIZACAO DE RECEBIMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CONT()
SAVE SCREEN TO TELA03
PTIT( "RELATàRIO DE CONTABILIZACAO DE RECEBIMENTOS")
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
SELE 7
USE CONTAS INDEX CONTAS
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
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eQUADRA  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO1+" (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO1+" : "
         GO TOP 
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@S99999"
         PIC[5]="@S1"  
         PIC[6]="@S20"         
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         CAB[4]="    "
         CAB[5]=" "   
         CAB[6]="    "        
         DBEDIT(6,14,6,75,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_QUADRA
         eNUM_INICIO = NUM_INICIO
         eNUM_FINAL  = NUM_FINAL
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO2+" (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO2+" : "
         GO TOP
         DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@S30"
         PIC[4]="@R99999"
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         CAB[4]="    "
         DBEDIT(6,08,6,76,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
   ENDIF     
eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja extrato em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CAMINHO\ARQUIVO: " GET eARQUIVO
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
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
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
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ou (0) para Todas ? " GET eCONTA PICT "@R 999"
   READ
   SELE 7
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA = 0
         EXIT
      ELSE
         PLQUA("01")
		 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("Conta NÆo Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eDIVIDE = .F.
eCASTOR = 0
IF sisCGC_CPF = "51665396000127"
   PLQUA("01")
   PPERG ( "Deseja aplica‡Æo de pencentual de redu‡Æo ? ")
   IF sisRESP = "S"
      eDIVIDE = .T.
      DO WHILE eCASTOR = 0
         PLQUA("01")
         @ 23,00 SAY "Informe o percentual a ser apresentado do total : " GET eCASTOR PICT "@R 999"
         READ
      ENDDO
   ENDIF
ENDIF
IF eQUADRA <> SPACE(5)
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   USE &eARQ INDEX &eIND3,&eIND2,&eIND1
   IF eCONTA = 0
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0)
   ELSE
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTA = eCONTA
   ENDIF
   GOTO TOP
ELSE
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
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = STR(eCLAS_PRO,5)
         eIND_QUAD = NUM_QUADRA
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         IF eCONTA = 0
            APPEND FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0)
         ELSE
            APPEND FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTA = eCONTA
         ENDIF
         GOTO TOP
         SELE 3
         SKIP
      ENDDO
      SELE 8
      IF eCONTA = 0
         APPEND FROM CAD_QUIT FOR CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0)
      ELSE
         APPEND FROM CAD_QUIT FOR CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTA = eCONTA
      ENDIF
      USE TEMP_PAR
      INDEX ON DATA_PGTO TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
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
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = STR(eCLAS_PRO,5)
         eIND_QUAD = NUM_BLOCO
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         IF eCONTA = 0
            APPEND FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0)
         ELSE
            APPEND FROM &eARQ FOR (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTA = eCONTA
         ENDIF
         GOTO TOP
         SELE 4
         SKIP
      ENDDO
      SELE 8
      IF eCONTA = 0
         APPEND FROM CAD_QUIT FOR CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0)
      ELSE
         APPEND FROM CAD_QUIT FOR CLASSE = STR(eCLAS_PRO,5) .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTA = eCONTA
      ENDIF
      USE TEMP_PAR
      INDEX ON DATA_PGTO TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
   ENDIF
ENDIF
SELE 8
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
   eDATA_PGTO = DATA_PGTO
   eSUB_PARC  = 0
   eSUB_MULTA = 0
   eSUB_DESC  = 0
   eSUB_PGTO  = 0
   eTOT_PARC  = 0
   eTOT_MULTA = 0
   eTOT_DESC  = 0
   eTOT_PGTO  = 0
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN004"
      @ 03,05  SAY sisLETRAE+"CONTABILIZACAO DE RECEBIMENTOS DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+IIF(eCONTA = 0," - TODAS "," - CONTA : "+STR(eCONTA,3))+sisLETRAC
      @ 04,00  SAY "Produto : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," ",+" -     Grupo    : "+ALLTRIM(eQUADRA))
      @ 04,50  SAY "Conta No. : "+STR(eCONTA,5)
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,02  SAY "No. IMOVEL"
      @ 06,15  SAY "DATA VENC."
      @ 06,27  SAY "TIPO"
      @ 06,34  SAY "NUM.PARC"
      @ 06,47  SAY "VLR. PARCELA"
      @ 06,67  SAY "MULTAS/JUROS"
      @ 06,87  SAY "DESCONTOS"
      @ 06,107 SAY "VLR. PAGO"
      @ 06,126 SAY "DATA PGTO."
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE .NOT. EOF() .AND. LIN <= 56
         DO WHILE eDATA_PGTO = DATA_PGTO .AND. .NOT. EOF() .AND. LIN <= 56
            @ LIN,02  SAY STR(PARTE,1)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)
            @ LIN,15  SAY DATA_VENC
            @ LIN,29  SAY TIPO_PARC
            @ LIN,36  SAY ALLTRIM(NUM_PARC)
            IF .NOT. eDIVIDE
               @ LIN,44  SAY VLR_PARC   PICT "@R 999,999,999,999.99"
               eVLR_PARC = VLR_PARC 
               eVLR_PGTO = VLR_PGTO
            ELSE
               eVLR_PARC = VLR_PARC * (eCASTOR / 100)
               eVLR_PGTO = VLR_PGTO * (eCASTOR / 100)
               @ LIN,44  SAY eVLR_PARC   PICT "@R 999,999,999,999.99"
            ENDIF
            eDESCONTO = 0
            eMULTA    = 0
            IF eVLR_PGTO <> eVLR_PARC
               IF eVLR_PGTO > eVLR_PARC
                  eMULTA    = eVLR_PGTO - eVLR_PARC
               ELSE
                  eDESCONTO = eVLR_PARC - eVLR_PGTO
               ENDIF
            ENDIF
            @ LIN,64  SAY eMULTA      PICT "@R 999,999,999,999.99"
            @ LIN,84  SAY eDESCONTO   PICT "@R 999,999,999,999.99"
            @ LIN,104 SAY eVLR_PGTO   PICT "@R 999,999,999,999.99"
            @ LIN,126 SAY DATA_PGTO
            eSUB_PARC  = eSUB_PARC  + eVLR_PARC
            eSUB_MULTA = eSUB_MULTA + eMULTA
            eSUB_DESC  = eSUB_DESC  + eDESCONTO
            eSUB_PGTO  = eSUB_PGTO  + eVLR_PGTO
            LIN = LIN + 1
            SKIP
         ENDDO
         IF eDATA_PGTO <> DATA_PGTO
            LIN = LIN + 1
            @ LIN,02  SAY "Sub-totais do Dia "+DTOC(eDATA_PGTO)+" : "
            @ LIN,44  SAY eSUB_PARC   PICT "@R 999,999,999,999.99"
            @ LIN,64  SAY eSUB_MULTA  PICT "@R 999,999,999,999.99"
            @ LIN,84  SAY eSUB_DESC   PICT "@R 999,999,999,999.99"
            @ LIN,104 SAY eSUB_PGTO   PICT "@R 999,999,999,999.99"
            LIN = LIN + 2
            eTOT_PARC  = eTOT_PARC  + eSUB_PARC
            eTOT_MULTA = eTOT_MULTA + eSUB_MULTA
            eTOT_DESC  = eTOT_DESC  + eSUB_DESC
            eTOT_PGTO  = eTOT_PGTO  + eSUB_PGTO
            eSUB_PARC  = 0
            eSUB_MULTA = 0
            eSUB_DESC  = 0
            eSUB_PGTO  = 0
            eDATA_PGTO = DATA_PGTO
         ENDIF
         DO PTECLA
         IF sisINTERR
            IF eSAIDA = "ARQUIVO"
               SET PRINT OFF
               SET DEVICE TO SCREEN
            ELSE
               SET DEVICE TO SCREEN
            ENDIF
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
      IF EOF()
         LIN = LIN + 2
         @ LIN,02  SAY "Total GERAL : "
         @ LIN,44  SAY eTOT_PARC   PICT "@R 999,999,999,999.99"
         @ LIN,64  SAY eTOT_MULTA  PICT "@R 999,999,999,999.99"
         @ LIN,84  SAY eTOT_DESC   PICT "@R 999,999,999,999.99"
         @ LIN,104 SAY eTOT_PGTO   PICT "@R 999,999,999,999.99"
      ENDIF
      @ 58,00 SAY REPLICATE("=",135)
      @ 59,104 SAY sisTIT3
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
************************ funcao *********************************
FUNCTION FUNC_CONS
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN = 5
            SAVE SCREEN TO T_PROP1
            ePROPRIET_1 = PROPRIET_1
            SELE 5
            SEEK ePROPRIET_1
            @ 12,13 CLEAR TO 14,67
            @ 12,13 TO       14,67 DOUBLE
			PCENTRA( "Nome do Proprietario No. 1",12)
            @ 13,15 GET CLIENTE
            CLEAR GETS
            SELE 1
            PPARA()
            RESTORE SCREEN FROM T_PROP1
         ELSE
            IF DB_COLUMN = 6
               SAVE SCREEN TO T_PROP2
               ePROPRIET_2 = PROPRIET_2
               SELE 5
               SEEK ePROPRIET_2
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Nome do Proprietario No. 2",12)
               @ 13,15 GET CLIENTE
               CLEAR GETS
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP2
            ELSE
               RET = 0
            ENDIF
         ENDIF
   ENDCASE
ENDIF
RETURN RET
*************************  fim  *********************************
