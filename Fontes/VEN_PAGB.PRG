*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa	: 
* Data        : 01/01/99
* Objetivo    : ALTERACAO DE VENCIMENTOS DA PARCELAS 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function VEN_PAGB()
SAVE SCREEN TO TELA03
PTIT( "ALTERA€¶O DOS VENCIMENTOS DE PARCELAS")
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
         @ 07,02 SAY sisUNID1+" : "
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
	 GOTO TOP
	 IF EOF()
	    PLQUA("01")
	    PMSGR( "Aten‡ƒo !",3)
            PMENSAG("NÆo H  "+sisUNID1+" Vendidos !")
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
	       PMSGR( "Aten‡Æo !",3)
               PMENSAG ( sisUNID1+" nÆo Vendido !")
	       PPARA()
	    ELSE
	       ePARTE = 0
	       F_VENB(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
	    ENDIF  
	 ELSE
	    eOPCAO = 0
	    PLQUA("01")
	    @ 23,00 SAY "Informe a Venda para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
	    READ
	    IF eOPCAO = 1
	       IF ePROPR_1 = 1
		  PLQUA("01")
		  PMSGR( "Aten‡Æo !",3)
                  PMENSAG ( sisUNID1+" nÆo Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 1
		  F_VENB(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
	       ENDIF  
	    ELSE
	       IF ePROPR_2 = 1
		  PLQUA("01")
		  PMSGR( "Aten‡Æo !",3)
                  PMENSAG ( sisUNID1+" nÆo Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 2
		  F_VENB(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_2)
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
	    PMSGR( "Aten‡ƒo !",3)
            PMENSAG("NÆo H  "+sisUNID2+" Vendidos !")
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
	    PMSGR( "Aten‡Æo !",3)
            PMENSAG ( sisUNID2+" nÆo Vendido !")
	    PPARA()
	 ELSE
	    F_VENB(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1) 
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
	    PMSGR( "Aten‡ƒo !",3)
	    PMENSAG("NÆo H  Salas Vendidas !")
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
	    PMSGR( "Aten‡Æo !",3)
	    PMENSAG("Imovel nÆo Vendido !")
	    PPARA()
	 ELSE
	    F_VENB(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1)
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
FUNCTION F_VENB
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
PLQUA("01")
PPERG ( "Mudan‡a de Vencimentos das Parcela (S/N) ? ")
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
   SELE 8
   SET FILTER TO CLASSE = STR(pCLASSE,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pPARTE .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. ATIVA
   GOTO TOP
   IF EOF()
      PLQUA("01")
      PMSGR( "Aten‡ƒo !",3)
      PMENSAG("NÆo h  Parcelas Neste Intervalo !")
      PPARA()
   ELSE
      DO WHILE .T.
         eFAZ     = .F.
	 eDIA     = 0
	 eDIA_ANT = VAL(SUBSTR(DTOC(DATA_VENC),1,2))
	 PLQUA("01")
	 @ 23,00 SAY "Mudar a Data de Vencimento de ("+ALLTRIM(STR(eDIA_ANT,2))+") para : " GET eDIA PICT "@R 999"
	 READ
         IF eDIA >= 1 .AND. eDIA <= 30 .AND. eDIA <> 0
	    eTAM_DIA  = LENNUM(INT(eDIA))
	    eDIA      = IIF(eTAM_DIA = 1,"0"+STR(eDIA,1),STR(eDIA,2))
	    EXIT
	 ELSE
	    PLQUA("01")
	    PMSGR( "Aten‡ƒo !",3)
            PMENSAG("Dia Inv lido !")
            PPARA()
	    LOOP
	 ENDIF 
      ENDDO
      eDIA_ANT = eDIA
      SELE 8
      GOTO TOP
      DO WHILE .NOT. EOF()
	 IF F_BLOQR()
	    eAUX_VENC = DATA_VENC
	    eMES      = VAL(SUBSTR(DTOC(eAUX_VENC),4,2))
	    eANO      = VAL(SUBSTR(DTOC(eAUX_VENC),7,4))
            IF eFAZ
               eDIA = eDIA_ANT
               eFAZ = .F.
	    ENDIF
	    IF eMES = 2 .AND. VAL(eDIA) > 28
               eDIA     = "28"
               eFAZ = .T.
            ENDIF
*  ?eDIA
*  ?? " - "+eDIA_ANT
	    eTAM_DIA  = LEN(ALLTRIM(eDIA))
	    eTAM_MES  = LENNUM(INT(eMES))
	    eDIA      = IIF(eTAM_DIA = 1,"0"+ALLTRIM(eDIA),ALLTRIM(eDIA))
	    eMES      = IIF(eTAM_MES = 1,"0"+STR(eMES,1),STR(eMES,2))
	    eANO      = STR(INT(eANO),4)
	    eAUX_VENC = CTOD(eDIA+"/"+eMES+"/"+eANO)
	    REPLACE DATA_VENC WITH eAUX_VENC
*  ?? " - "+DTOC(eAUX_VENC)
*  PPARA()
	    UNLOCK
	 ENDIF
	 SELE 8
	 SKIP
      ENDDO
      F_LACESSO(sisUSUARIO,"MENU PARCELAS","MUDANCA DE VENC. : "+ALLTRIM(STR(pCLASSE,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pPARTE,1)))
      PLQUA("01")
	  PMSGR( "Altera‡Æo Conclu¡da !",3)
   ENDIF
ENDIF
RETURN 0
***************************** fim ***********************************
     
