*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : 
* Data        : 01/01/99
* Objetivo    : VENDA DE UNIDADES IMOBILIARIAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function VEN_IMOV() 
SAVE SCREEN TO TELA03
PTIT( "VENDA DE UNIDADES ")
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
SELE 7
USE INDICES INDEX INDICES
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
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eBLOCO  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      DO WHILE .T.
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
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 = 1),(PROPRIET_1 = 1 .OR. PROPRIET_2 = 1))
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Aten‡Æo !",3)
         PMENSAG("NÆo H  "+sisUNID1+" Dispon¡veis neste Produto.")
	 PPARA()
      ELSE
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
	    IF ePROPR_1 <> 1
	       PLQUA("01")
	       PMSGR( "Aten‡Æo !",3)
               PMENSAG ( sisUNID1+" ja Vendido !")
	       PPARA()
	    ELSE
	       ePARTE = 0
	       F_VENDA(eTIPO_PRO,eCLAS_PRO,eNUM_IMOVEL,0)
	    ENDIF
	 ELSE
	    eOPCAO = 0
	    PLQUA("01")
	    @ 23,00 SAY "Informe a Venda para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
	    READ
	    IF eOPCAO = 1
	       IF ePROPR_1 <> 1
		  PLQUA("01")
		  PMSGR( "Aten‡Æo !",3)
                  PMENSAG ( sisUNID1+" ja Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 1
		  F_VENDA(eTIPO_PRO,eCLAS_PRO,eNUM_IMOVEL,1)
	       ENDIF
	    ELSE
	       IF ePROPR_2 <> 1
		  PLQUA("01")
		  PMSGR( "Aten‡Æo !",3)
                  PMENSAG ( sisUNID1+" ja Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 2
		  F_VENDA(eTIPO_PRO,eCLAS_PRO,eNUM_IMOVEL,2)
	       ENDIF
	    ENDIF
	 ENDIF
      ENDIF
	 PLQUA("01")
         PPERG(" Deseja  Continuar  no  Mesmo  Produto  (S/N) ?  ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    SELE 3
	    SET FILTER TO
	    GOTO TOP    
	    LOOP
	 ELSE
	    EXIT
	 ENDIF      
      ENDDO
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      DO WHILE .T.
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
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. PROPRIET_1 = 1
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Aten‡Æo !",3)
         PMENSAG("NÆo H  "+sisUNID2+" Dispon¡veis neste Produto.")
	 PPARA()
      ELSE
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
	 IF ePROPR_1 <> 1
	    PLQUA("01")
	    PMSGR( "Aten‡Æo !",3)
            PMENSAG ( sisUNID2+" ja Vendido !")
	    PPARA()
	 ELSE
	    F_VENDA(eTIPO_PRO,eCLAS_PRO,eNUM_IMOVEL,0)
	 ENDIF
      ENDIF
	 PLQUA("01")
         PPERG(" Deseja  Continuar  no  Mesmo  Produto  (S/N) ?  ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    SELE 4
	    SET FILTER TO
	    GOTO TOP   	    
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      DO WHILE .T.
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
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. PROPRIET_1 = 1
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Aten‡Æo !",3)
         PMENSAG("NÆo H  Unidades Dispon¡veis neste Produto.")
	 PPARA()
      ELSE
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
	 IF ePROPR_1 <> 1
	    PLQUA("01")
	    PMSGR( "Aten‡Æo !",3)
	    PMENSAG("Imovel ja Vendido !")
	    PPARA()
	 ELSE
	    F_VENDA(eTIPO_PRO,eCLAS_PRO,eNUM_IMOVEL,0)
	 ENDIF
      ENDIF
	 PLQUA("01")
         PPERG(" Deseja  Continuar  no  Mesmo  Produto  (S/N) ?  ")
	 IF sisRESP = "S"
	    @ 06,00 CLEAR TO 21,79
	    SELE 4
	    SET FILTER TO
	    GOTO TOP   	    
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
FUNCTION F_VENDA
PARAMETERS pT_PRO,pCLASSE,pUNIDADE,pM_UNID
eMSG = "VENDA DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pM_UNID=0,"Proprietario Unico",IIF(pM_UNID=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9)
PLQUA("01")
PPERG ( "Confirma Unidade (S/N) ? ")
IF sisRESP = "N"
   RETURN 0
ENDIF
DO WHILE .T.
   SELE 5
   USE CONTATO INDEX CONTATO,NUM_CLIE
   GOTO TOP
   ePESQUISA = SPACE(49)
   @ 10,02 CLEAR TO 11,79 
   @ 11,02 SAY "Nome :" GET ePESQUISA PICT "@! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   READ
   ePESQUISA = ALLTRIM(ePESQUISA)
   SEEK ePESQUISA
   IF EOF()
      PLQUA("01")
      PMSGR( "Cliente Invalido !",3)
      PLQUA("01")
      PPERG ( "Deseja Cadastrar o Novo Cliente (S/N) ? ")
      IF sisRESP = "S"
	 SAVE SCREEN TO INC_CLIE
	 INCLUI()
	 RESTORE SCREEN FROM INC_CLIE
	 LOOP
      ENDIF
   ENDIF
   DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
   AFIELDS(CAMPO,.T.,LENS)
   CAMPO[2]="CPF_CGC"
   LENS[2]=14
   PIC[1]="@S50"
   PIC[2]="@S14"
   CAB[1]="  NOME"
   CAB[2]="CGC/CPF"
   DBEDIT(11,05,11,79,CAMPO,"KEYS_44",PIC,CAB," "," "," ")
   eNUM_CLIE = NUM_CLIE
   IF eNUM_CLIE = 1 
      PLQUA("01")
      PMSGR( "Cliente Invalido !",3)
      LOOP
   ELSE
      PLQUA("01")
      PPERG ( "Confirma (S/N) ? ")
      IF sisRESP = "S"
	 EXIT
      ELSE
	 LOOP
      ENDIF
   ENDIF
ENDDO
eDT_VENDA  = CTOD("")
eVALOR     = 0
eSALDO     = 0
ePARCELAS  = 0
eVLR_PARC  = 0
ePRI_VENC  = CTOD("")
eINTERMEDIA = 0
eSINAL      = 0
eNUM_INTERM = 0
eNUM_SINAL  = 0
eNUM_FLUXOS = 0
eMULTA      = 0
eCOMISSAO   = 0
eSINAL_P    = .F.
ePERID_COMI = "a.m."
DO WHILE .T.
   @ 13,02 SAY "Data da Venda : " GET eDT_VENDA
   @ 14,02 SAY "Valor Total   : " GET eVALOR      PICT "@R 999,999,999,999.99"
   @ 15,02 SAY "Vlr. do Sinal : " GET eSINAL      PICT "@R 999,999,999,999.99"
   READ
   IF eSINAL <> 0
      PLQUA("01")
      PPERG ( "Deseja Sinal Parcelado (S/N) ? ")
      IF sisRESP = "S"
         eSINAL_P = .T.
         IF eSINAL <> 0
            DO WHILE .T.
               @ 15,40 SAY "Num. Parcelas : " GET eNUM_SINAL PICT "@R 999"
               READ
               IF eNUM_SINAL <> 0
                  EXIT
               ENDIF
            ENDDO
            DECLARE NUMERO_S[eNUM_SINAL+1],VLR_SINAL[eNUM_SINAL+1],DATA_S[eNUM_SINAL+1],PER[eNUM_SINAL+1]
            AFILL(NUMERO_S,0)
            AFILL(VLR_SINAL,0)
            AFILL(DATA_S,CTOD(""))
            AFILL(PER,0)
            eAUX = 1
            DO WHILE .T.
               DO WHILE eAUX <= eNUM_SINAL
                  DO WHILE .T.
                     PLQUA("01")
                     @ 23,00 SAY "Vlr. Sinal "+ALLTRIM(STR(eAUX))+"/"+ALLTRIM(STR(eNUM_SINAL))+": " GET VLR_SINAL[eAUX] PICT "999,999,999,999.99"
                     @ 23,35 SAY "Vencimento : " GET DATA_S[eAUX]
                     @ 23,62 SAY "% Com:" GET PER[eAUX] PICT "@R 999.99"
                     READ
                     IF VLR_SINAL[eAUX] = 0 .OR. DATA_S[eAUX] = CTOD("")
                        LOOP
                     ELSE
                        PLQUA("01")
                        EXIT
                     ENDIF
                  ENDDO
                  NUMERO_S[eAUX] = eAUX
                  eAUX = eAUX + 1
               ENDDO
               eAUX  = 1
               eSOMA = 0
               DO WHILE eAUX <= eNUM_SINAL
                  eSOMA = eSOMA + VLR_SINAL[eAUX]
                  eAUX  = eAUX + 1
               ENDDO
               IF (INT(eSOMA*100) >= INT(eSINAL*100)+5) .OR. (INT(eSOMA*100) <= INT(eSINAL*100)-5)
                  PLQUA("01")
                  PMSGR( "Aten‡Æo !",3)
                  PMENSAG("Valor das Parcelas nao Conferem !, Digite Novamente .")
                  PPARA()
                  PLQUA("01")
                  @ 23,22 SAY "Digitado : " GET eSOMA    PICT "@R 999,999,999.9999"
                  @ 23,50 SAY "Original : " GET eSINAL   PICT "@R 999,999,999.9999"
                  CLEAR GETS
                  PPARA()
                  eAUX = 1
                  LOOP
               ELSE
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ELSE 
         eSINAL_P = .F.
      ENDIF
   ENDIF
   eSALDO = eVALOR - eSINAL
   @ 16,02 SAY "Saldo         : " GET eSALDO      PICT "@R 999,999,999,999.99"
   CLEAR GETS
   @ 17,02 SAY "Vlr. Intermed.: " GET eINTERMEDIA PICT "@R 999,999,999,999.99"
   READ
   IF eINTERMEDIA <> 0
      eSALDO = eSALDO - eINTERMEDIA
      DO WHILE .T.
	 @ 17,40 SAY "Num. Parcelas : " GET eNUM_INTERM PICT "@R 999"
	 READ
	 IF eNUM_INTERM <> 0
	    EXIT
	 ENDIF
      ENDDO
      DECLARE NUMERO_I[eNUM_INTERM+1],VLR_INTERM[eNUM_INTERM+1],DATA_I[eNUM_INTERM+1],OBS[eNUM_INTERM+1]
      AFILL(NUMERO_I,0)
      AFILL(VLR_INTERM,0)
      AFILL(DATA_I,CTOD(""))
      AFILL(OBS,SPACE(10))
      eAUX = 1
      DO WHILE .T.
	 DO WHILE eAUX <= eNUM_INTERM
	    DO WHILE .T.
	       PLQUA("01")
	       @ 23,00 SAY "Vlr.Interm "+ALLTRIM(STR(eAUX))+"/"+ALLTRIM(STR(eNUM_INTERM))+": " GET VLR_INTERM[eAUX] PICT "999,999,999,999.99"
	       @ 23,35 SAY "Vencimento : " GET DATA_I[eAUX]
	       @ 23,62 SAY "Obs.: " GET OBS[eAUX]
	       READ
	       IF VLR_INTERM[eAUX] = 0 .OR. DATA_I[eAUX] = CTOD("")
		  LOOP
	       ELSE
		  PLQUA("01")
		  EXIT
	       ENDIF
	    ENDDO
	    NUMERO_I[eAUX] = eAUX
	    eAUX = eAUX + 1
	 ENDDO
	 eAUX  = 1
	 eSOMA = 0
	 DO WHILE eAUX <= eNUM_INTERM
	    eSOMA = eSOMA + VLR_INTERM[eAUX]
	    eAUX  = eAUX + 1
	 ENDDO
         IF (INT(eSOMA*100) >= INT(eINTERMEDIA*100)+5) .OR. (INT(eSOMA*100) <= INT(eINTERMEDIA*100)-5)
	    PLQUA("01")
	    PMSGR( "Aten‡Æo !",3)
	    PMENSAG("Valor das Parcelas nao Conferem !, Digite Novamente .")
	    PPARA()
            PLQUA("01")
            @ 23,22 SAY "Digitado : " GET eSOMA         PICT "@R 999,999,999.9999"
            @ 23,50 SAY "Original : " GET eINTERMEDIA  PICT "@R 999,999,999.9999"
            CLEAR GETS
            PPARA()
	    eAUX = 1
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   ENDIF
   DECLARE vVLR_PARC[10],vNUM_PARC[10],vPRI_VENC[10]
   AFILL(vVLR_PARC,0)
   AFILL(vNUM_PARC,0)
   AFILL(vPRI_VENC,CTOD(""))
   IF eSALDO <> 0
      DO WHILE .T.
	 eVLR_PARC = eSALDO
	 @ 18,02 SAY "Tot. Parcelas.: " GET eVLR_PARC PICT "@R 999,999,999,999.99"
	 CLEAR GETS
	 @ 18,40 SAY "Num. Fluxos   : " GET eNUM_FLUXOS PICT "@R 999"
	 READ
	 IF eNUM_FLUXOS <> 0
	    EXIT
	 ENDIF
      ENDDO
      ePARCELAS = 0
      eAUX = 1
      DO WHILE .T.
	 DO WHILE eAUX <= eNUM_FLUXOS
	    DO WHILE .T.
	       PLQUA("01")
	       @ 23,00 SAY "Vlr.Parcela - Fluxo "+ALLTRIM(STR(eAUX))+": " GET vVLR_PARC[eAUX] PICT "999,999,999.99"
	       @ 23,39 SAY "Quantidade : " GET vNUM_PARC[eAUX] PICT "@R 999"
	       @ 23,58 SAY "1o. Venc.: " GET vPRI_VENC[eAUX]
	       READ
               IF VAL(SUBSTR(DTOC(vPRI_VENC[eAUX]),1,2)) >= 32
                  PLQUA("01")
                  PMSGR( "Aten‡Æo !",3)
                  PMENSAG("Vencimentos aceitos apenas do dia 1 ao dia 31.")
                  PPARA()
                  LOOP
               ENDIF
	       IF vVLR_PARC[eAUX] = 0 .OR. vPRI_VENC[eAUX] = CTOD("") .OR. vNUM_PARC[eAUX] = 0
		  LOOP
	       ELSE
		  PLQUA("01")
		  EXIT
	       ENDIF
	    ENDDO
	    eAUX = eAUX + 1
	 ENDDO
	 eAUX  = 1
	 eSOMA     = 0
	 ePARCELAS = 0
	 DO WHILE eAUX <= eNUM_FLUXOS
	    eSOMA     = eSOMA + (vVLR_PARC[eAUX]*vNUM_PARC[eAUX])
	    ePARCELAS = ePARCELAS + vNUM_PARC[eAUX]
	    eAUX      = eAUX + 1
	 ENDDO
         IF (INT(eSOMA*100) >= INT(eVLR_PARC*100)+5) .OR. (INT(eSOMA*100) <= INT(eVLR_PARC*100)-5)
	    PLQUA("01")
	    PMSGR( "Aten‡Æo !",3)
            PMENSAG("Valores Conferem !.")
            @ 23,22 SAY "Digitado : " GET eSOMA     PICT "@R 999,999,999.9999"
            @ 23,50 SAY "Original : " GET eVLR_PARC PICT "@R 999,999,999.9999"
            CLEAR GETS
	    PPARA()
	    eAUX = 1
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   ENDIF
   SELE 7
   USE INDICES INDEX INDICES   
   eINDICE = SPACE(12)
   @ 19,02 SAY "Indice        : "
   GO TOP
   DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="XXXXXXXXXXX"
   CAB[1]="    "
   DBEDIT(19,18,19,31,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eINDICE = NOME
   ePERIODO = SPACE(15)
   @ 19,35 say "Periodicidade   : "
   @ 19,54 PROMPT "MENSAL   "
   @ 19,54 PROMPT "ANUAL    "
   @ 19,54 PROMPT "ANUAL12  "
   @ 19,54 PROMPT "SEMESTRAL"
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
      CASE sisOP = 1
         ePERIODO = "MENSAL"
      CASE sisOP = 2
         ePERIODO = "ANUAL"
      CASE sisOP = 3
         ePERIODO = "ANUAL12"
      CASE sisOP = 4
         ePERIODO = "SEMESTRAL"
   ENDCASE
   SELE 7
   USE CORRETOR INDEX CORRETOR
   GOTO TOP
   eCORRETOR = SPACE(12)
   @ 20,02 SAY "Corretor      : "
   GO TOP
   DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R 999"
   PIC[2]="@S12"
   CAB[1]="    "
   CAB[2]="    "
   DBEDIT(20,18,20,38,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCORRETOR = CODIGO
   @ 21,02 SAY "Multa         : " GET eMULTA PICT "@R 999.99"
   @ 21,40 SAY "ComissÆo      : " GET eCOMISSAO PICT "@R 999.999"
   READ
   DO WHILE .T.
      @ 21,68 GET ePERID_COMI PICT "@S4"
      READ
      IF ePERID_COMI = "a.d." .OR. ePERID_COMI = "a.m."
         EXIT
      ELSE
         PLQUA("01")
	 PMSGR( "Aten‡Æo !",1)
         PMENSAG("Formato Inv lido, utilize [a.d.] ou [a.m.], tecle ENTER")
         PPARA()
         LOOP
      ENDIF
   ENDDO
   eOBSERVACAO = SPACE(60)
   PLQUA("01")
   @ 23,02 SAY "Observa‡Æo : " GET eOBSERVACAO
   READ
   GOTO TOP
   PLQUA("01")
   PPERG ( "Confirma os Dados da Venda (S/N) ? ")
   IF sisRESP = "S"
      IF eDT_VENDA < CTOD("01/08/1993")
	 PLQUA("01")
	 PPERG ( "Confirma os ConversÆo em Cr$/CR$ (S/N) ? ")
	 IF sisRESP = "S"
	    eCONV_CREAL = .T.
	 ELSE
	    eCONV_CREAL = .F.
	 ENDIF
      ELSE
	 eCONV_CREAL = .F.
      ENDIF
      IF eDT_VENDA < CTOD("01/07/1994")
	 PLQUA("01")
	 PPERG ( "Confirma os ConversÆo em Real (S/N) ? ")
	 IF sisRESP = "S"
	    eCONV_REAL = .T.
	 ELSE
	    eCONV_REAL = .F.
	 ENDIF
      ELSE
	 eCONV_REAL = .F.
      ENDIF
      PLQUA("01")
      PPERG ( "Deseja habilitar protesto para todas as parcelas (S/N) ? ")
      IF sisRESP = "S"
         ePROTESTO = .T.
      ELSE
         ePROTESTO = .F.
      ENDIF
      * Cadastramento da Venda
      PLQUA("01")
      PMENSAG("Aguarde ...")
      SELE 6
      APPEND BLANK
      IF F_BLOQR()
	 REPLACE CLASSE      WITH STR(eCLAS_PRO,5),;
		 QUADRA      WITH IIF(eTIPO_PRO=1,eQUADRA,eBLOCO),;
		 NUM_IMOVEL  WITH eNUM_IMOVEL
	 REPLACE PARTE       WITH ePARTE,;
		 NUM_CLIE    WITH eNUM_CLIE,;
		 DATA_VENDA  WITH eDT_VENDA
	 REPLACE SINAL       WITH eSINAL,;
		 PARCELAS    WITH ePARCELAS,;
		 VLR_PARCEL  WITH eVLR_PARC
	 REPLACE PRI_VENC    WITH vPRI_VENC[1],;
		 ULT_VENC    WITH CTOD(""),;
		 INTERMEDIA  WITH eINTERMEDIA
	 REPLACE NUM_INTERM  WITH eNUM_INTERM,;
		 INDICE      WITH eINDICE,;
		 ULT_CORREC  WITH eDT_VENDA
         REPLACE CORRETOR    WITH eCORRETOR,;
                 PERIODO     WITH ePERIODO,;
                 OBSERVACAO  WITH eOBSERVACAO
	 IF eSINAL_P
	    REPLACE SINAL_PAR WITH .T.
	 ELSE
	    REPLACE SINAL_PAR WITH .F.
	 ENDIF
	 UNLOCK
      ENDIF
      * Inclusao das Parcelas do Financiamento - Normal
      SELE 8
      eIND_CLAS = STR(eCLAS_PRO,5)
      eIND_QUAD = IIF(eTIPO_PRO=1,eQUADRA,eBLOCO)
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      USE &eARQ INDEX &eIND1,&eIND2,&eIND3
      GOTO TOP
      IF eSALDO <> 0
	 eNUM_PARC = 1
	 eAUX_FLU = 1
	 DO WHILE eAUX_FLU <= eNUM_FLUXOS
	    eAUX = 1
	    eAUX_VENC = vPRI_VENC[eAUX_FLU]
	    DO WHILE eAUX <= vNUM_PARC[eAUX_FLU]
	       APPEND BLANK
	       IF F_BLOQR()
		  REPLACE CLASSE      WITH STR(eCLAS_PRO,5),;
			  QUADRA      WITH IIF(eTIPO_PRO=1,eQUADRA,eBLOCO),;
			  NUM_IMOVEL  WITH eNUM_IMOVEL
		  REPLACE PARTE       WITH ePARTE,;
			  NUM_PARC    WITH STR(eNUM_PARC,5)
		  REPLACE TIPO_PARC   WITH "N",;
			  DATA_VENC   WITH eAUX_VENC,;
			  ATIVA       WITH .T.
                  REPLACE CONTABIL    WITH .T.,;
                          MULTA       WITH eMULTA,;
                          COMISSAO    WITH eCOMISSAO
                  REPLACE PERID_COMI  WITH ePERID_COMI
                  IF ePROTESTO
                     REPLACE PROTESTO WITH ePROTESTO
                  ENDIF
		  eVLR_AUX = 0
		  IF eCONV_CREAL
		     eVLR_AUX = vVLR_PARC[eAUX_FLU]/1000
		  ELSE
		     eVLR_AUX = vVLR_PARC[eAUX_FLU]
		  ENDIF
		  IF eAUX_VENC >= CTOD("01/07/1994") .AND. eCONV_REAL
		     eVLR_AUX = eVLR_AUX/2750
		  ELSE
		     eVLR_AUX = eVLR_AUX
		  ENDIF
		  REPLACE VLR_PARC     WITH eVLR_AUX,;
			  VLR_ORIGIN   WITH eVLR_AUX
		  UNLOCK
	       ENDIF
	       eNUM_PARC = eNUM_PARC + 1
	       eAUX      = eAUX      + 1
	       eDIA      = 0
	       eDIA      = VAL(SUBSTR(DTOC(vPRI_VENC[eAUX_FLU]),1,2))
	       eMES      = 0
	       eANO      = VAL(SUBSTR(DTOC(eAUX_VENC),7,4))
	       IF VAL(SUBSTR(DTOC(eAUX_VENC),4,2)) + 1 = 13
		  eMES = 1
		  eANO = eANO + 1
	       ELSE
		  eMES = VAL(SUBSTR(DTOC(eAUX_VENC),4,2)) + 1
		  IF eMES = 2
		     IF eDIA > 28
			eDIA = 28
		     ENDIF
		  ENDIF
                  IF eMES = 4
                     IF eDIA > 30
                        eDIA = 30
		     ENDIF
		  ENDIF
                  IF eMES = 6
                     IF eDIA > 30
                        eDIA = 30
		     ENDIF
		  ENDIF
                  IF eMES = 9
                     IF eDIA > 30
                        eDIA = 30
		     ENDIF
		  ENDIF
                  IF eMES = 11
                     IF eDIA > 30
                        eDIA = 30
		     ENDIF
		  ENDIF
	       ENDIF
	       eTAM_DIA  = LENNUM(INT(eDIA))
	       eTAM_MES  = LENNUM(INT(eMES))
	       eDIA      = IIF(eTAM_DIA = 1,"0"+STR(eDIA,1),STR(eDIA,2))
	       eMES      = IIF(eTAM_MES = 1,"0"+STR(eMES,1),STR(eMES,2))
	       eANO      = STR(INT(eANO),4)
	       eAUX_VENC = CTOD(eDIA+"/"+eMES+"/"+eANO)
	    ENDDO
	    eAUX_FLU = eAUX_FLU + 1
	 ENDDO
	 SELE 6
	 IF F_BLOQR()
	    REPLACE ULT_VENC    WITH eAUX_VENC
	    UNLOCK
	 ENDIF
      ENDIF
      SELE 8
      * Inclusao das Parcelas do Financiamento - Intermediarias
      IF eINTERMEDIA <> 0
	 eAUX = 1
	 DO WHILE eAUX <= eNUM_INTERM
	    APPEND BLANK
	    IF F_BLOQR()
	       REPLACE CLASSE      WITH STR(eCLAS_PRO,5),;
		       QUADRA      WITH IIF(eTIPO_PRO=1,eQUADRA,eBLOCO),;
		       NUM_IMOVEL  WITH eNUM_IMOVEL
	       REPLACE PARTE       WITH ePARTE,;
		       NUM_PARC    WITH STR(eAUX,5)
	       REPLACE TIPO_PARC   WITH "I",;
		       DATA_VENC   WITH DATA_I[eAUX],;
		       OBS         WITH OBS[eAUX]
	       REPLACE ATIVA       WITH .T.
               REPLACE CONTABIL    WITH .T.,;
                       MULTA       WITH eMULTA,;
                       COMISSAO    WITH eCOMISSAO
               REPLACE PERID_COMI  WITH ePERID_COMI
               IF ePROTESTO
                  REPLACE PROTESTO WITH ePROTESTO
               ENDIF
	       eVLR_AUX = 0
	       IF eCONV_CREAL
		  eVLR_AUX = VLR_INTERM[eAUX]/1000
	       ELSE
		  eVLR_AUX = VLR_INTERM[eAUX]
	       ENDIF
	       IF DATA_I[eAUX] >= CTOD("01/07/1994") .AND. eCONV_REAL
		  eVLR_AUX = eVLR_AUX/2750
	       ELSE
		  eVLR_AUX = eVLR_AUX
	       ENDIF
	       REPLACE VLR_PARC     WITH eVLR_AUX,;
		       VLR_ORIGIN   WITH eVLR_AUX
	       UNLOCK
	    ENDIF
	    eAUX = eAUX + 1
	 ENDDO
      ENDIF
      SELE 8
      * Inclusao das Parcelas do Financiamento - Sinal
      IF eSINAL <> 0 .AND. eSINAL_P
	 eAUX = 1
	 DO WHILE eAUX <= eNUM_SINAL
	    APPEND BLANK
	    IF F_BLOQR()
	       REPLACE CLASSE      WITH STR(eCLAS_PRO,5),;
		       QUADRA      WITH IIF(eTIPO_PRO=1,eQUADRA,eBLOCO),;
		       NUM_IMOVEL  WITH eNUM_IMOVEL
	       REPLACE PARTE       WITH ePARTE,;
		       NUM_PARC    WITH STR(eAUX,5)
	       REPLACE TIPO_PARC   WITH "S",;
		       DATA_VENC   WITH DATA_S[eAUX],;
		       PERCENTUAL  WITH PER[eAUX]
	       REPLACE ATIVA       WITH .T.
               REPLACE CONTABIL    WITH .T.,;
                       MULTA       WITH eMULTA,;
                       COMISSAO    WITH eCOMISSAO
               REPLACE PERID_COMI  WITH ePERID_COMI
               IF ePROTESTO
                  REPLACE PROTESTO WITH ePROTESTO
               ENDIF
               eVLR_AUX = 0
	       IF eCONV_CREAL
		  eVLR_AUX = VLR_SINAL[eAUX]/1000
	       ELSE
		  eVLR_AUX = VLR_SINAL[eAUX]
	       ENDIF
	       IF DATA_S[eAUX] >= CTOD("01/07/1994") .AND. eCONV_REAL
		  eVLR_AUX = eVLR_AUX/2750
	       ELSE
		  eVLR_AUX = eVLR_AUX
	       ENDIF
	       REPLACE VLR_PARC     WITH eVLR_AUX,;
		       VLR_ORIGIN   WITH eVLR_AUX
	       UNLOCK
	    ENDIF
	    eAUX = eAUX + 1
	 ENDDO
      ENDIF
      SELE 1
      IF F_BLOQR()
	 DO CASE
	    CASE pM_UNID = 0
	       REPLACE PROPRIET_1 WITH eNUM_CLIE
	    CASE pM_UNID = 1
	       REPLACE PROPRIET_1 WITH eNUM_CLIE
	    CASE pM_UNID = 2
	       REPLACE PROPRIET_2 WITH eNUM_CLIE
	    OTHERWISE
	       PLQUA("01")
	       PMENSAG("ERRO GRAVE, CONTACTE O SUPORTE IMEDIATAMENTE, IMPRIMA A TELA")
	       PPARA()
	       PPARA()
	 ENDCASE
	 UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"VENDA DE IMOVEIS","VENDA DO IMOVEL : "+ALLTRIM(STR(eCLAS_PRO,5))+"/"+ALLTRIM(IIF(eTIPO_PRO=1,eQUADRA,eBLOCO))+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+ALLTRIM(STR(ePARTE,1)))
      PLQUA("01")
      PMSGR( "Venda Concluida !",3)
      SELE 1
      EXIT
   ELSE
      EXIT
   ENDIF
ENDDO
RETURN 0
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION KEYS_44
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
******************************************************************
