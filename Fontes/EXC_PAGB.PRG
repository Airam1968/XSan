*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_PAGB.PRG
* Data        : 01/01/99
* Objetivo    : EXCLUSAO0 DE PAGAMENTO DE PARCELAS BALCAO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXC_PAGB()
SAVE SCREEN TO TELA03
PTIT( "EXCLUS«O DE PAGAMENTO DA PARCELA")
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
eQUADRA = SPACE(1)
eBLOCO  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      DO WHILE .T. 
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
	 eQUADRA     = NUM_QUADRA
	 eNUM_INICIO = NUM_INICIO
	 eNUM_FINAL  = NUM_FINAL
	 @ 07,02 SAY "Lote    : "
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
	 GOTO TOP
	 IF EOF()
	    PLQUA("01")
	    PMSGR( "Atená∆o !",3)
            PMENSAG("N∆o H† "+sisUNID1+" Vendidos !")
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
               PMENSAG ( sisUNID1+" nao Vendido !")
	       PPARA()
	    ELSE
	       ePARTE = 0
	       F_EXCB(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
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
                  PMENSAG ( sisUNID1+" nao Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 1
		  F_EXCB(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
	       ENDIF  
	    ELSE
	       IF ePROPR_2 = 1
		  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" nao Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 2
		  F_EXCB(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_2)
	       ENDIF  
	    ENDIF
	 ENDIF
	 PLQUA("01")
         PPERG ( "Deseja Continuar no Mesmo Produto (S/N) ? ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   CASE eTIPO_PRO = 2
      DO WHILE .T.
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
            PMENSAG("N∆o H† "+sisUNID2+" Vendidos !")
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
            PMENSAG ( sisUNID2+" nao Vendido !")
	    PPARA()
	 ELSE
	    F_EXCB(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1) 
	 ENDIF
	 PLQUA("01")
         PPERG ( "Deseja Continuar no Mesmo Produto (S/N) ? ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   CASE eTIPO_PRO = 3
      DO WHILE .T.
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
	    PMENSAG("N∆o H† Salas Vendidas !")
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
	    F_EXCB(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1)
	 ENDIF
	 PLQUA("01")
	 PPERG ( "Deseja Continuar no Mesmo Empreendimento (S/N) ? ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_EXCB
PARAMETERS pCLASSE,pQUADRA,pUNIDADE,pPARTE,pNUM_CLIE
eMSG = "RELACAO DAS PARCELAS DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pPARTE=0,"Proprietario Unico",IIF(pPARTE=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9)
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK pNUM_CLIE
@ 08,02 SAY "Cliente : "+CLIENTE
eCLIENTE = CLIENTE
eSEXO    = SEXO
SELE 8
eIND_CLAS = STR(pCLASSE,5)
eIND_QUAD = pQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
USE &eARQ INDEX &eIND2,&eIND1,&eIND3
GOTO TOP
SET FILTER TO CLASSE = STR(pCLASSE,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pPARTE
GOTO TOP
DECLARE CAMPO[15],PIC[15],CAB[15],LENS[15]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[1]="NUM_PARC"
CAMPO[2]="TIPO_PARC"
CAMPO[3]="DATA_VENC"
CAMPO[4]="VLR_PARC"
CAMPO[5]="DATA_PGTO"
CAMPO[6]="VLR_PGTO"
CAMPO[7]="COD_BANCO"
CAMPO[8]="ORIGEM"
CAMPO[9]="PROTESTO"
CAMPO[10]="CONTABIL"
CAMPO[11]="OBS"
CAMPO[12]="MULTA"
CAMPO[13]="COMISSAO"
CAMPO[14]="PERCENTUAL" 
CAMPO[15]="CONTA"
LENS[1]=5
LENS[2]=3
LENS[3]=8
LENS[4]=19
LENS[5]=8
LENS[6]=19
LENS[7]=20
LENS[8]=1
LENS[9]=1
LENS[10]=1
LENS[11]=10
LENS[12]=10
LENS[13]=10
LENS[14]=10
LENS[15]=3
PIC[1]="@R99999"
PIC[2]="XXX"
PIC[3]="99/99/99"
PIC[4]="@R 99,999,999,999.99"
PIC[5]="99/99/99"
PIC[6]="@R 99,999,999,999.99"
PIC[7]="@R 999999999999999"
PIC[8]="@!X"
PIC[9]="L"
PIC[10]="L"
PIC[11]="XXXXXXXXXX"
PIC[12]="@R 999.99"
PIC[13]="@R 999.99"
PIC[14]="@R 999.99"
PIC[15]="@R 999"
CAB[1] ="Parcela"
CAB[2] ="Tipo(N/I)"
CAB[3] ="Venc."
CAB[4] ="Parcela"
CAB[5] ="Pago"
CAB[6] ="Vlr. Pagamento"
CAB[7] ="Cod. Banco"
CAB[8] ="Origem"
CAB[9] ="Protesto"
CAB[10]="Contabil"
CAB[11]="Obs."
CAB[12]="Multa"
CAB[13]="Tx. perm."
CAB[14]="Tx. Comis."
@ 10,00 TO 21,79 DOUBLE
DBEDIT(11,01,21,78,CAMPO,"FUNC_EXCB",PIC,CAB,CHR(196),CHR(179),CHR(205))
RETURN 0
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_EXCB
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 SAVE SCREEN TO TELA04
         SET CURSOR ON
         PLQUA("01")
         PPERG ( "Cancela o Pagamento da Parcela (S/N) ? ")
         IF sisRESP = "S"
            IF .NOT. ATIVA
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
               PMENSAG("N∆o á Possável Cancelar um Pagamento de uma Parcela Inativa !")
               PPARA()
            ELSE
               eDATA_PGTO = DATA_PGTO
               eVLR_PGTO  = VLR_PGTO
               IF F_BLOQR()
                  REPLACE DATA_PGTO WITH CTOD(""),;
                          VLR_PGTO  WITH 0,;
                          ORIGEM    WITH SPACE(1)
                  UNLOCK
                  eNUM_PARC  = NUM_PARC
                  eTIPO_PARC = TIPO_PARC
               ENDIF
               PLQUA("01")
               PMSGR( "Cancelamento Concluádo !",3)
               F_LACESSO(sisUSUARIO,"VENDA DE IMOVEIS","EXCL.PAGAMENTO : "+ALLTRIM(STR(pCLASSE,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pPARTE,1))+" PARC : "+ALLTRIM(eNUM_PARC)+" TIPO : "+ALLTRIM(TIPO_PARC))
               SELE 8
	    ENDIF
	 ENDIF
	 SET CURSOR OFF
	 RESTORE SCREEN FROM TELA04
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
     
