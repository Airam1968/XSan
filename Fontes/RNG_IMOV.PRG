*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : RNG_IMOV.PRG
* Data        : 01/01/99
* Objetivo    : RENEGOCIACAO DE UNIDADES IMOBILIARIAS 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
SAVE SCREEN TO TELA03
PTIT( "RENEGOCIACAO DA VENDA")
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
USE RENEGOCI INDEX RENEGOCI
GOTO TOP
SET FILTER TO TIPO_RENEG = "R"
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
   @ 05,02 SAY sisGRUPO1+" : "
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
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = SPACE(5)
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
		 PMENSAG("N∆o H† "+sisUNID1+" Dispon°veis neste Produto.")
         PPARA()
      ELSE
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[5]="PROPRIET_1"
         CAMPO[6]="PROPRIET_2"
         LENS[5] =5
         LENS[6] =5
         PIC[1]  ="@R99999"
         PIC[2]  ="@R99999"
         PIC[3]  ="@R99999"
         PIC[4]  ="@R99999"
         PIC[5]  ="@R99999"
         PIC[6]  ="@R99999"
         CAB[1]  ="     "
         CAB[2]  ="     "
         CAB[3]  ="     "
         CAB[4]  ="     "
         CAB[5]  ="     "
         CAB[6]  ="     "
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
               F_RENEG(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,0)
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
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 1
                  F_RENEG(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,1)
               ENDIF
            ELSE
               IF ePROPR_2 = 1
                  PLQUA("01")
                  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 2
                  F_RENEG(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,2)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
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
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† "+sisUNID2+" Dispon°veis neste Produto.")
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
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG ( sisUNID2+" N∆o Vendido !")
            PPARA()
         ELSE
            F_RENEG(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
         ENDIF
      ENDIF
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
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
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Atená∆o !",3)
		 PMENSAG("N∆o H† Imáveis Dispon°veis neste Empreendimento.")
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
         IF ePROPR_1 = 1
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
			PMENSAG("Imovel N∆o Vendido !")
            PPARA()
         ELSE
            F_RENEG(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
         ENDIF
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_RENEG
PARAMETERS pT_PRO,pQUADRA,pUNIDADE,pM_UNID
eMSG = "RENEGOCIACAO DA VENDA DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pM_UNID=0,"Proprietario Unico",IIF(pM_UNID=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9)
PLQUA("01")
PPERG ( "Confirma o Item (S/N) ? ")
IF sisRESP = "N"
   RETURN 0
ENDIF
PLQUA("01")
PPERG ( "Deseja habilitar protesto para todas as parcelas (S/N) ? ")
IF sisRESP = "S"
   ePROTESTO = .T.
ELSE
   ePROTESTO = .F.
ENDIF
SELE 6
GOTO TOP
SEEK STR(pT_PRO,5)+pQUADRA+pUNIDADE+STR(pM_UNID,1)
eNUM_CLIE = NUM_CLIE
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK eNUM_CLIE
@ 11,02 SAY "Nome :" GET CLIENTE
CLEAR GETS
DO WHILE .T.
   ePERID_COMI = "a.m."
   eDATA_RENE  = CTOD("")
   ePARCELAS   = 0
   eVLR_PARC   = 0
   ePRI_VENC   = CTOD("")
   eINTERMEDIA = 0
   @ 13,02 SAY "Data Renegoc. : " GET eDATA_RENE
   READ
   DECLARE vVLR_PARC[10],vNUM_PARC[10],vPRI_VENC[10]
   AFILL(vVLR_PARC,0)
   AFILL(vNUM_PARC,0)
   AFILL(vPRI_VENC,CTOD(""))
   eNUM_FLUXOS = 0
   eVLR_PARC   = 0
   DO WHILE .T.
      @ 14,02 SAY "Tot. Parcelas.: " GET eVLR_PARC PICT "@R 999,999,999,999.99"
      @ 14,40 SAY "Num. Fluxos   : " GET eNUM_FLUXOS PICT "@R 999"
      READ
      IF eNUM_FLUXOS <> 0
         EXIT
      ENDIF
   ENDDO
   ePARCELAS = 0
   eAUX = 1
   DO WHILE eAUX <= eNUM_FLUXOS
      DO WHILE .T.
         PLQUA("01")
         @ 23,00 SAY "Vlr.Parcela - Fluxo "+ALLTRIM(STR(eAUX))+": " GET vVLR_PARC[eAUX] PICT "999,999,999.99"
         @ 23,39 SAY "Quantidade : " GET vNUM_PARC[eAUX] PICT "@R 999"
         @ 23,58 SAY "1o. Venc.: " GET vPRI_VENC[eAUX]
         READ
         IF VAL(SUBSTR(DTOC(vPRI_VENC[eAUX]),1,2)) >= 31
            PLQUA("01")
			PMSGR( "Atená∆o !",3)
			PMENSAG("Vencimentos aceitos apenas do dia 1 ao dia 30.")
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
   @ 15,02 SAY "Vlr. Intermed.: " GET eINTERMEDIA PICT "@R 999,999,999,999.99"
   READ
   eNUM_INTERM = 0
   IF eINTERMEDIA <> 0
      DO WHILE .T.
         @ 15,40 SAY "Num. Parcelas : " GET eNUM_INTERM PICT "@R 999"
         READ
         IF eNUM_INTERM <> 0
            EXIT
         ENDIF
      ENDDO
      DECLARE NUMERO[eNUM_INTERM+1],VLR[eNUM_INTERM+1],DATA[eNUM_INTERM+1],OBS[eNUM_INTERM+1]
      AFILL(NUMERO,0)
      AFILL(VLR,0)
      AFILL(DATA,CTOD(""))
      AFILL(OBS,SPACE(10))
      eAUX = 1
      DO WHILE .T.
         DO WHILE eAUX <= eNUM_INTERM
            DO WHILE .T.
               PLQUA("01")
               @ 23,00 SAY "Intermediaria "+ALLTRIM(STR(eAUX))+"/"+ALLTRIM(STR(eNUM_INTERM))+" : " GET VLR[eAUX] PICT "999,999,999.99"
               @ 23,35 SAY "Vencimento : " GET DATA[eAUX]
               @ 23,62 SAY "Obs.: " GET OBS[eAUX]
               READ
               IF VLR[eAUX] = 0 .OR. DATA[eAUX] = CTOD("")
                  LOOP
               ELSE
                  PLQUA("01")
                  EXIT
               ENDIF
            ENDDO
            NUMERO[eAUX] = eAUX
            eAUX = eAUX + 1
         ENDDO
         eAUX  = 1
         eSOMA = 0
         DO WHILE eAUX <= eNUM_INTERM
            eSOMA = eSOMA + VLR[eAUX]
            eAUX  = eAUX + 1
         ENDDO
         IF INT(eSOMA*100) <> INT(eINTERMEDIA*100)
            PLQUA("01")
			PMSGR( "Atencáo !",3)
			PMENSAG("Valor das Parcelas nao Conferem !, Digite Novamente .")
            PPARA()
            eAUX = 1
            PLQUA("01")
            PPERG ( "Cancela (S/N) ? ")
            IF sisRESP = "N"
               LOOP
            ELSE
               PLQUA("01")
			   PMSGR( "Renegociaá∆o Cancelada !",3)
			   RETURN 0
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF
   eMULTA = 0
   eCOMISSAO = 0
   @ 21,02 SAY "Multa a.m.    : " GET eMULTA PICT "@R 999.99"
   @ 21,40 SAY "Comissáo      : " GET eCOMISSAO PICT "@R 999.999"
   READ
   DO WHILE .T.
      @ 21,65 GET ePERID_COMI PICT "@S4"
      READ
      IF ePERID_COMI = "a.d." .OR. ePERID_COMI = "a.m."
         EXIT
      ELSE
         PLQUA("01")
		 PMSGR( "Atená∆o !",1)
		 PMENSAG("Formato inválido, utilize [a.d.] ou [a.m.], tecle ENTER")
         PPARA()
         LOOP
      ENDIF
   ENDDO
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PPERG ( "Confirma Dados da Renegociaá∆o da Venda (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  PMENSAG("Aguarde ...")
      SELE 8
      eIND_CLAS = STR(pT_PRO,5)
      eIND_QUAD = pQUADRA
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      USE &eARQ INDEX &eIND1,&eIND2,&eIND3
      GOTO TOP
      IF F_BLOQA()
         DELETE ALL FOR CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID .AND. DATA_VENC >= eDATA_RENE .AND. DATA_PGTO = CTOD("")
         GOTO TOP
         REPLACE ALL ATIVA WITH .F. FOR CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID        && .AND. DATA_VENC < eDATA_RENE 
         UNLOCK
      ENDIF
      eREAL = .F. 
      IF eDATA_RENE <= CTOD("30/06/94") 
         PLQUA("01")
         PPERG ( "Deseja Conversao de Parcelas em Real (S/N) ? ")
         eVLR_REAL = 0
         eREAL = .F.
         IF sisRESP = "S"
            eREAL = .T.
            DO WHILE .T.
               PLQUA("01")
               @ 23,00 SAY "Valor da Parcela em R$ : " GET eVLR_REAL PICT "@R 999,999,999.99"
               READ
               IF eVLR_REAL = 0
                  LOOP
               ELSE
                  PLQUA("01")
                  @ 23,50 SAY "R$ : " GET eVLR_REAL PICT "@R 999,999,999.99"
                  CLEAR GETS 
                  PPERG ( "Confirma o Valor em Real (S/N) ? ")
                  IF sisRESP = "S"
                     EXIT
                  ELSE
                     LOOP
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      eNUM_RNG = 0
      SELE 7 
      GOTO TOP
      SEEK STR(pT_PRO,5)+pQUADRA+pUNIDADE+STR(pM_UNID,1)
      IF FOUND()
         DO WHILE CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID
            eNUM_RNG = eNUM_RNG + 1
            SKIP
         ENDDO
      ENDIF
      APPEND BLANK
      IF F_BLOQR()
         REPLACE CLASSE      WITH STR(pT_PRO,5),;
                 QUADRA      WITH pQUADRA,;
                 NUM_IMOVEL  WITH pUNIDADE
         REPLACE PARTE       WITH pM_UNID,;
                 NUM_CLIE    WITH eNUM_CLIE,;
                 DATA_RENEG  WITH eDATA_RENE
         REPLACE VLR_PARCEL  WITH eVLR_PARC,;
                 PRI_VENC    WITH ePRI_VENC
         REPLACE INTERMEDIA  WITH eINTERMEDIA,;
                 NUM_INTERM  WITH eNUM_INTERM,;
                 TIPO_RENEG  WITH "R"
         REPLACE NUM_RENEG   WITH eNUM_RNG,;
                 PARCELAS    WITH ePARCELAS
         UNLOCK
      ENDIF
      * Inclusao de Parcelas Normais
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
               REPLACE TIPO_PARC   WITH "R"+ALLTRIM(STR(eNUM_RNG,1)),;
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
               eVLR_AUX = vVLR_PARC[eAUX_FLU]
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
      SELE 8
      * Inclusao das Parcelas do Financiamento - Intermediarias
      IF eINTERMEDIA <> 0
         eAUX = 1
         DO WHILE eAUX <= eNUM_INTERM
            APPEND BLANK
            IF F_BLOQR()
               REPLACE CLASSE      WITH STR(pT_PRO,5),;
                       QUADRA      WITH pQUADRA,;
                       NUM_IMOVEL  WITH pUNIDADE
               REPLACE PARTE       WITH pM_UNID,;
                       NUM_PARC    WITH STR(eAUX,5)
               REPLACE TIPO_PARC   WITH "I",;
                       DATA_VENC   WITH DATA[eAUX]
               REPLACE VLR_PARC    WITH VLR[eAUX],;
                       OBS         WITH OBS[eAUX],;
                       ATIVA       WITH .T.
               REPLACE VLR_ORIGIN  WITH VLR[eAUX],;
                       MULTA       WITH eMULTA,;
                       COMISSAO    WITH eCOMISSAO
               REPLACE CONTABIL    WITH .T.
               REPLACE PERID_COMI  WITH ePERID_COMI
               IF ePROTESTO
                  REPLACE PROTESTO WITH ePROTESTO
               ENDIF
               UNLOCK
            ENDIF
            eAUX = eAUX + 1
         ENDDO
      ENDIF
      PLQUA("01")
      PMSGR( "Renegociaá∆o Concluida !",3)
      F_LACESSO(sisUSUARIO,"ROTINAS ESPECIAIS","RENEGOCIACAO IMOVEL : "+ALLTRIM(STR(pT_PRO,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pM_UNID,1))) 
      EXIT
   ELSE
      PLQUA("01")
	  PMSGR( "Renegociaá∆o Cancelada !",3)
      EXIT
   ENDIF
ENDDO
RETURN 0
*************************  fim  *********************************
