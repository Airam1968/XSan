*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_PAGP.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DA CURVA DE PARCELAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
function con_pagp()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE PARCELAS DO PRODUTO")
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
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
DO WHILE .T.
   eQUADRA = SPACE(1)
   eBLOCO  = SPACE(1)
   DO CASE
      CASE eTIPO_PRO = 1
	 * Consulta de Loteamentos
	 SELE 3
	 SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
	 GOTO TOP
	 eQUADRA   = 0
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
	 GO TOP
	 DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 PIC[4]="@S99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 CAB[4]="    "
	 DBEDIT(6,14,6,45,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
	 eQUADRA     = NUM_QUADRA
	 eNUM_INICIO = NUM_INICIO
	 eNUM_FINAL  = NUM_FINAL
         @ 07,02 SAY sisUNID1+" : "
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
	 GOTO TOP
	 IF EOF()
	    PLQUA("01")
		PMSGR( "Atená∆o !",3)
			PMENSAG("N∆o H† Unidades Dispon°veis !")
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
               PMENSAG("Unid. nao Vendida !")
	       PPARA()
	    ELSE
	       ePARTE = 0
	       F_PAGP(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
	    ENDIF
	 ELSE
	    eOPCAO = 0
	    PLQUA("01")
	    @ 23,00 SAY "Informe a Venda para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
	    READ
	    IF eOPCAO = 1
	       IF ePROPR_1 = 1
		  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG("Unid. nao Vendida !")
		  PPARA()
	       ELSE
		  ePARTE = 1
		  F_PAGP(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
	       ENDIF
	    ELSE
	       IF ePROPR_2 = 1
		  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG("Unid. nao Vendida !")
		  PPARA()
	       ELSE
		  ePARTE = 2
		  F_PAGP(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_2)
	       ENDIF
	    ENDIF
	 ENDIF
      CASE eTIPO_PRO = 2
	 * Consulta de Apartamentos
	 SELE 4
	 SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
	 GOTO TOP
	 eBLOCO    = 0
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
	 eBLOCO      = NUM_BLOCO
	 eQUANTIDADE = QUANTIDADE
         @ 07,02 SAY sisUNID2+" : "
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1))
	 GOTO TOP
	 IF EOF()
	    PLQUA("01")
		PMSGR( "Atená∆o !",3)
			PMENSAG("N∆o H† Unidades Dispon°veis !")
	    PPARA()
	    CLOSE DATABASES
	    RESTORE SCREEN FROM TELA03
	    RETURN nil
	 ENDIF
	 DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
	 AFIELDS(CAMPO,.T.,LENS)
	 CAMPO[5]="PROPRIET_1"
	 LENS[5] =5
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 PIC[4]="@R99999"
	 PIC[5]="@R99999"
         CAB[1]=" Produto "
         CAB[2]="Grupo"
         CAB[3]="Unid."
	 CAB[4]="Status"
	 CAB[5]="Proprietario"
	 DBEDIT(7,14,7,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
	 eNUM_IMOVEL = NUM_IMOVEL
	 eSTATUS     = STATUS
	 ePROPR_1    = PROPRIET_1
	 ePARTE      = 0
	 IF ePROPR_1 = 1
	    PLQUA("01")
	    PMSGR( "Atená∆o !",3)
            PMENSAG("Unid. nao Vendida !")
	    PPARA()
	 ELSE
	    F_PAGP(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1) 
	 ENDIF
      CASE eTIPO_PRO = 3
	 * Consulta de Salas Comerciais
	 SELE 4
	 SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
	 GOTO TOP
	 eBLOCO    = 0
	 @ 06,02 SAY "Bloco   : "
	 GO TOP
	 DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 CAB[1]="    "
	 CAB[2]="    "
	 CAB[3]="    "
	 DBEDIT(6,10,6,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
	 eBLOCO      = NUM_BLOCO
	 eQUANTIDADE = QUANTIDADE
	 @ 07,02 SAY "Sala    : "
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1))
	 GOTO TOP
	 IF EOF()
	    PLQUA("01")
		PMSGR( "Atená∆o !",3)
			PMENSAG("N∆o H† Unidades Dispon°veis !")
	    PPARA()
	    CLOSE DATABASES
	    RESTORE SCREEN FROM TELA03
	    RETURN nil
	 ENDIF
	 DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
	 AFIELDS(CAMPO,.T.,LENS)
	 CAMPO[5]="PROPRIET_1"
	 LENS[5] =5
	 PIC[1]="@R99999"
	 PIC[2]="@R99999"
	 PIC[3]="@R99999"
	 PIC[4]="@R99999"
	 PIC[5]="@R99999"
	 CAB[1]="Empreend."
	 CAB[2]="Bloco"
	 CAB[3]="Sala"
	 CAB[4]="Status"
	 CAB[5]="Proprietario"
	 DBEDIT(7,14,7,64,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
	 eNUM_IMOVEL = NUM_IMOVEL
	 eSTATUS     = STATUS
	 ePROPR_1    = PROPRIET_1
	 ePARTE      = 0
	 IF ePROPR_1 = 1
	    PLQUA("01")
	    PMSGR( "Atená∆o !",3)
	    PMENSAG("Imovel nao Vendido !")
	    PPARA()
	 ELSE
	    F_PAGP(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1)
	 ENDIF
      OTHERWISE
	 * Consulta de Casa/Terrenos/etc...
	 DO PDESENV
   ENDCASE
   PLQUA("01")
   PPERG ( "Deseja Continuar no mesmo Empreendimento (S/N) ? ")
   IF sisRESP = "S"
      @ 06,00 CLEAR TO 21,79
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_PAGP
PARAMETERS pCLASSE,pQUADRA,pUNIDADE,pPARTE,pNUM_CLIE
eMSG = "CONSULTA DAS PARCELAS DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pPARTE=0,"Proprietario Unico",IIF(pPARTE=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9)
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK pNUM_CLIE
@ 08,02 SAY "Cliente : "+CLIENTE
SELE 8
eIND_CLAS = STR(pCLASSE,5)
eIND_QUAD = pQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eTIPO_CON = SPACE(1)
PLQUA("01") 
@ 23,00 SAY "Informe a Consulta por Ordem de Parcela ou Vencimento (P/V) ? " GET eTIPO_CON PICT "@!X" VALID eTIPO_CON $("VP")
READ
IF eTIPO_CON = "V" 
   USE &eARQ INDEX &eIND2,&eIND1,&eIND3
   GOTO TOP
   SET FILTER TO CLASSE = STR(pCLASSE,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pPARTE
   GOTO TOP
ELSE
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   GOTO TOP
   SET FILTER TO CLASSE = STR(pCLASSE,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pPARTE
   GOTO TOP
ENDIF
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("N∆o H† Parcelas Neste Financiamento !")
   PPARA()
   RETURN 0
ENDIF
DECLARE CAMPO[13],PIC[13],CAB[13],LENS[13]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[1] ="NUM_PARC"
CAMPO[2] ="TIPO_PARC"
CAMPO[3] ="DATA_VENC"
CAMPO[4] ="VLR_PARC"
CAMPO[5] ="COD_BANCO" 
CAMPO[6] ="ORIGEM"
CAMPO[7] ="PROTESTO"
CAMPO[8] ="CONTABIL"
CAMPO[9] ="OBS"
CAMPO[10]="MULTA"
CAMPO[11]="COMISSAO"
CAMPO[12]="PERID_COMI"
CAMPO[13]="PERCENTUAL"
LENS[1] =5
LENS[2] =3
LENS[3] =8
LENS[4] =19
LENS[5] =12
LENS[6] =1
LENS[7] =1
LENS[8] =1
LENS[9] =10
LENS[10]=10
LENS[11]=10
LENS[12]=4
LENS[13]=5
PIC[1] ="@R 99999"
PIC[2] ="XXX"
PIC[3] ="99/99/99"
PIC[4] ="@R 999,999,999,999.99"
PIC[5] ="@R 999999999999" 
PIC[6] ="@!X"
PIC[7] ="L"
PIC[8] ="L"
PIC[9] ="XXXXXXXXXX"
PIC[10]="@R 999.99"
PIC[11]="@R 999.99"
PIC[12]="XXXX"
PIC[13]="@R 999.99"
CAB[1] ="No.Parcela"
CAB[2] ="Tipo (N/I)"
CAB[3] ="Vencimento"
CAB[4] ="Valor"
CAB[5] ="Cod. Banco" 
CAB[6] ="Origem"
CAB[7] ="Protesto"
CAB[8] ="Contabil"
CAB[9] ="Obs."
CAB[10]="Multa"
CAB[11]="Comissáo"
CAB[12]="Perid."
CAB[13]="Comissáo Corretor"
@ 10,00 TO 21,79 DOUBLE
DBEDIT(11,01,21,78,CAMPO,"F_C_PAGP",PIC,CAB,CHR(196),CHR(179),CHR(205))
PLQUA("01")
PPERG ( "Deseja Totalizaá∆o (S/N) ? ")
IF sisRESP = "S"
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
   SELE 8
   GOTO TOP
   DO WHILE .NOT. EOF()
      eDESCONTO = 0
      eMULTA    = 0
      IF VLR_PGTO <> VLR_PARC .AND. VLR_PGTO <> 0
         IF VLR_PGTO > VLR_PARC
            eMULTA    = VLR_PGTO - VLR_PARC
         ELSE
            eDESCONTO = VLR_PARC - VLR_PGTO
         ENDIF
      ELSE
         IF VLR_PGTO = 0
            eTOT_3PARC = eTOT_3PARC + VLR_PARC
         ENDIF
      ENDIF
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
      SKIP
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
   ENDDO
   IF EOF()
      PLIMPA()
      @ 05,02   SAY "Total Geral (Cr$) : "
      @ 06,02   SAY "Total Parcelas    : " GET eTOT_1PARC   PICT "@R 999,999,999,999.99"
      @ 07,02   SAY "Total Pagamentos  : " GET eTOT_1PGTO   PICT "@R 999,999,999,999.99"
      @ 08,02   SAY "Total Multa       : " GET eTOT_1MULTA  PICT "@R 999,999,999,999.99"
      @ 09,02   SAY "Total Descontos   : " GET eTOT_1DESC   PICT "@R 999,999,999,999.99"
      @ 11,02   SAY "Total Geral (R$)  :  "
      @ 12,02   SAY "Total Parcelas    : " GET eTOT_2PARC   PICT "@R 999,999,999,999.99"
      @ 13,02   SAY "Total Pagamentos  : " GET eTOT_2PGTO   PICT "@R 999,999,999,999.99"
      @ 14,02   SAY "Total Multa       : " GET eTOT_2MULTA  PICT "@R 999,999,999,999.99"
      @ 15,02   SAY "Total Descontos   : " GET eTOT_2DESC   PICT "@R 999,999,999,999.99"
      @ 17,02   SAY "Saldo Devedor     :  "
      @ 18,02   SAY "Total             : " GET eTOT_3PARC   PICT "@R 999,999,999,999.99"
   ENDIF
   CLEAR GETS
   PPARA()
   SELE 8
   GOTO TOP
ENDIF
RETURN 0
*************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_C_PAGP
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
        IF DB_COLUMN >= 1
	   IF F_BLOQR()
	      SAVE SCREEN TO TELA999
	      SET CURSOR ON
	      FLD_NAME = CAMPO[DB_COLUMN]
	      FLD_PIC  = PIC[DB_COLUMN]
	      @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
	      READ
	      SET CURSOR OFF
	      UNLOCK
	      RESTORE SCREEN FROM TELA999
	   ENDIF
	ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
