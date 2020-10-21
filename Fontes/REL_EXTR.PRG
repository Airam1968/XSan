*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_EXTR.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE EXTRATO DE FINANCIAMENTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_EXTR()
SAVE SCREEN TO TELA03
PTIT( "RELAT‡RIO EXTRATO DE FINANCIAMENTO")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
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
         DBEDIT(6,14,6,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      PLQUA("01")
      PPERG ( "Deseja escolher a Unidade (S/N) ? ")
      IF sisRESP = "S"
         @ 07,02 SAY sisUNID1+" : "
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
         GOTO TOP
         IF EOF()
            PLQUA("01")
			PMSGR( "Atená∆o !",3)
			PMENSAG("N∆o H† "+sisUNID1+" Dispon°veis !")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ENDIF
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         CAMPO[6]="PROPRIET_2"
         LENS[5] =5
         LENS[6] =5
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@R99999"
         PIC[5]="@R99999"
         PIC[6]="@R99999"
         CAB[1]="     "
         CAB[2]="     "
         CAB[3]="     "
         CAB[4]="     "
         CAB[5]="     "
         CAB[6]="     "
         DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTATUS     = STATUS
         ePROPR_1    = PROPRIET_1
         ePROPR_2    = PROPRIET_2
         IF eSTATUS = 0
            IF ePROPR_1 = 1
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
               PMENSAG ( sisUNID1+" N∆o Vendido !")
               PPARA()
            ELSE
               ePARTE = 0
               SELE 1
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 0
               GOTO TOP
            ENDIF
         ELSE
            eOPCAO = 0
            PLQUA("01")
            @ 23,00 SAY "Informe se (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
            READ
            IF eOPCAO = 1
               IF ePROPR_1 = 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
		  PPARA()
	       ELSE
                  ePARTE = 1
                  SELE 1
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 1
                  GOTO TOP
	       ENDIF
	    ELSE
	       IF ePROPR_2 = 1
		  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 2
                  SELE 1
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 1
                  GOTO TOP
	       ENDIF
	    ENDIF
	 ENDIF
      ELSE
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
         GOTO TOP
         eNUM_IMOVEL = 0
         eSTATUS     = 0
         ePROPR_1    = 0
         ePROPR_2    = 0
         ePARTE      = 0
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
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
      DBEDIT(6,08,6,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      PLQUA("01")
      PPERG ( "Deseja escolher a Unidade (S/N) ? ")
      IF sisRESP = "S"
         @ 07,02 SAY sisUNID2+" : "
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
         GOTO TOP
         IF EOF()
            PLQUA("01")
		PMSGR( "Atená∆o !",3)
		PMENSAG("N∆o H† "+sisUNID2+" Dispon°veis !")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ENDIF
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         CAMPO[6]="PROPRIET_2"
         LENS[5] =5
         LENS[6] =5
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@R99999"
         PIC[5]="@R99999"
         PIC[6]="@R99999"
         CAB[1]="     "
         CAB[2]="     "
         CAB[3]="     "
         CAB[4]="     "
         CAB[5]="     "
         CAB[6]="     "
         DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eNUM_IMOVEL = NUM_IMOVEL
         eSTATUS     = STATUS
         ePROPR_1    = PROPRIET_1
         ePARTE      = 0
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. STATUS = 0
         GOTO TOP
      ELSE
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
         GOTO TOP
         eNUM_IMOVEL = 0
         eSTATUS     = 0
         ePROPR_1    = 0
         ePARTE      = 0
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE

eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
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
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO
PLQUA("01")
PPERG ( "Deseja Relatorio por Periodo (S/N) ? ")
IF sisRESP = "S"
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
ENDIF
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
         PMSGR( "Atená∆o !",3)
         PMENSAG("Conta N∆o Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
SELE 8
eIND_CLAS = STR(eCLAS_PRO,5)
eIND_QUAD = eQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
USE &eARQ INDEX &eIND2,&eIND3,&eIND1
GOTO TOP
SELE 1
GOTO TOP
eNUM_IMOVEL = NUM_IMOVEL
eSTATUS     = STATUS
ePROPR_1    = PROPRIET_1
ePROPR_2    = PROPRIET_2
*ePARTE      = 0
SELE 1
DO WHILE .NOT. EOF()
   SELE 8
   IF eCONTA = 0
      IF sisRESP = "N"
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE
         GOTO TOP
      ELSE
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ((DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .OR. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM))
         GOTO TOP
      ENDIF
   ELSE
      IF sisRESP = "N"
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. CONTA = eCONTA
         GOTO TOP
      ELSE
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ((DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .OR. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)) .AND. CONTA = eCONTA
         GOTO TOP
      ENDIF
   ENDIF
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† Extrato Deste Per°odo !")
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
      eSUB_1PARC  = 0
      eSUB_1MULTA = 0
      eSUB_1DESC  = 0
      eSUB_1PGTO  = 0
      eTOT_1PARC  = 0
      eTOT_1MULTA = 0
      eTOT_1DESC  = 0
      eTOT_1PGTO  = 0
      eSUB_2PARC  = 0
      eSUB_2MULTA = 0
      eSUB_2DESC  = 0
      eSUB_2PGTO  = 0
      eTOT_2PARC  = 0
      eTOT_2MULTA = 0
      eTOT_2DESC  = 0
      eTOT_2PGTO  = 0
      eTOT_3PARC  = 0
      DO WHILE .NOT. EOF()
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,120 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,121 SAY "Relat: SAN004"
         @ 03,35  SAY sisLETRAE+"EXTRATO DE FINANCIAMENTO"+IIF(eCONTA <> 0," - CONTA No. "+ALLTRIM(STR(eCONTA,5))," ")+sisLETRAC
         @ 04,00  SAY "Produto : "+ALLTRIM(STR(eCLAS_PRO,5))+" - "+ALLTRIM(eDESC_PRO)+" Grupo : "+ALLTRIM(eQUADRA)+" Unid. No. : "+ALLTRIM(eNUM_IMOVEL)+" Parte : "+STR(ePARTE,1)
         SELE 5
         GOTO TOP
         IF (ePARTE = 1 .OR. ePARTE = 0)
            SEEK ePROPR_1
            @ 05,00  SAY "Proprietario   : "+STR(ePROPR_1,5)+" - "+ALLTRIM(CLIENTE)
         ELSE
            IF ePARTE = 2
               SEEK ePROPR_2
               @ 05,00  SAY "Proprietario   : "+STR(ePROPR_2,5)+" - "+ALLTRIM(CLIENTE)
            ENDIF
         ENDIF
         SELE 8
         @ 06,00  SAY REPLICATE("=",135)
         @ 07,02  SAY "No. "+sisUNID1
         @ 07,15  SAY "DATA VENC."
         @ 07,27  SAY "TIPO"
         @ 07,34  SAY "PARC."
         @ 07,40  SAY "CONTA"
         @ 07,47  SAY "VLR. PARCELA"
         @ 07,63  SAY "DATA PGTO"
         @ 07,79  SAY "VLR. PAGO"
         @ 07,99  SAY "MULTAS/JUROS"
         @ 07,119 SAY "DESCONTOS"
         @ 07,132 SAY "O"
         @ 07,134 SAY "$"
         @ 08,00 SAY REPLICATE("=",135)
         LIN = 9
         DO WHILE .NOT. EOF() .AND. LIN <= 56
            @ LIN,02  SAY ALLTRIM(NUM_IMOVEL)
            @ LIN,15  SAY DATA_VENC
            @ LIN,29  SAY TIPO_PARC
            @ LIN,35  SAY ALLTRIM(NUM_PARC)
            @ LIN,40  SAY CONTA      PICT "@R 999"
            @ LIN,44  SAY VLR_PARC   PICT "@R 999,999,999.99"
            @ LIN,61  SAY "|"
            @ LIN,63  SAY DATA_PGTO            
            @ LIN,75  SAY VLR_PGTO   PICT "@R 9,999,999,999.99"            
            eDESCONTO = 0
            eMULTA    = 0
            IF VLR_PGTO <> VLR_PARC .AND. VLR_PGTO <> 0 .AND. DATA_PGTO <> CTOD("") 
               IF VLR_PGTO > VLR_PARC
                  eMULTA    = VLR_PGTO - VLR_PARC
               ELSE
                  eDESCONTO = VLR_PARC - VLR_PGTO
               ENDIF
            ELSE
               IF VLR_PGTO = 0 .AND. DATA_PGTO = CTOD("") .AND. ATIVA
                  eTOT_3PARC = eTOT_3PARC + VLR_PARC
               ENDIF
            ENDIF
            @ LIN,95  SAY eMULTA     PICT "@R 9,999,999,999.99"
            @ LIN,115 SAY eDESCONTO  PICT "@R 9,999,999,999.99"
            @ LIN,132 SAY ALLTRIM(ORIGEM) PICT "X"
            @ LIN,134 SAY ALLTRIM(MOEDA) PICT "X"
            IF DATA_VENC <= CTOD("30/06/1994") .AND. DATA_PGTO <= CTOD("30/06/1994")
               eSUB_1PARC  = eSUB_1PARC  + VLR_PARC
               eSUB_1MULTA = eSUB_1MULTA + eMULTA
               eSUB_1DESC  = eSUB_1DESC  + eDESCONTO
               eSUB_1PGTO  = eSUB_1PGTO  + VLR_PGTO
            ELSE
               eSUB_2PARC  = eSUB_2PARC  + VLR_PARC
               eSUB_2MULTA = eSUB_2MULTA + eMULTA
               eSUB_2DESC  = eSUB_2DESC  + eDESCONTO
               eSUB_2PGTO  = eSUB_2PGTO  + VLR_PGTO
            ENDIF
            LIN = LIN + 1
            SKIP
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
         eTOT_1PARC  = eTOT_1PARC  + eSUB_1PARC
         eTOT_1MULTA = eTOT_1MULTA + eSUB_1MULTA
         eTOT_1DESC  = eTOT_1DESC  + eSUB_1DESC
         eTOT_1PGTO  = eTOT_1PGTO  + eSUB_1PGTO
         eTOT_2PARC  = eTOT_2PARC  + eSUB_2PARC
         eTOT_2MULTA = eTOT_2MULTA + eSUB_2MULTA
         eTOT_2DESC  = eTOT_2DESC  + eSUB_2DESC
         eTOT_2PGTO  = eTOT_2PGTO  + eSUB_2PGTO
         eSUB_1PARC  = 0
         eSUB_1MULTA = 0
         eSUB_1DESC  = 0
         eSUB_1PGTO  = 0
         eSUB_2PARC  = 0
         eSUB_2MULTA = 0
         eSUB_2DESC  = 0
         eSUB_2PGTO  = 0
         IF EOF()
            @ LIN,00 SAY REPLICATE("-",135)
            LIN = LIN + 1
            @ LIN,02  SAY "Total Geral (Cr$) :  "
            @ LIN,40  SAY eTOT_1PARC   PICT "@R 999,999,999,999.99"
            @ LIN,73  SAY eTOT_1PGTO   PICT "@R 999,999,999,999.99"
            @ LIN,93  SAY eTOT_1MULTA  PICT "@R 999,999,999,999.99"
            @ LIN,113 SAY eTOT_1DESC   PICT "@R 999,999,999,999.99"
            LIN = LIN + 1
            @ LIN,02  SAY "Total Geral (R$)  :  "
            @ LIN,40  SAY eTOT_2PARC   PICT "@R 999,999,999,999.99"
            @ LIN,73  SAY eTOT_2PGTO   PICT "@R 999,999,999,999.99"
            @ LIN,93  SAY eTOT_2MULTA  PICT "@R 999,999,999,999.99"
            @ LIN,113 SAY eTOT_2DESC   PICT "@R 999,999,999,999.99"
            LIN = LIN + 2
            @ LIN,02  SAY "Saldo Devedor     :  "
            @ LIN,40  SAY eTOT_3PARC   PICT "@R 999,999,999,999.99"
         ENDIF
         @ 58,00 SAY REPLICATE("=",135)
         @ 59,104 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
      ENDDO
   ENDIF
   SELE 1
   SKIP
   eNUM_IMOVEL = NUM_IMOVEL
   eSTATUS     = STATUS
   ePROPR_1    = PROPRIET_1
   ePARTE      = 0
ENDDO
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
