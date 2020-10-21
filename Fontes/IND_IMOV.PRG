*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : IND_IMOV.PRG
* Data        : 01/01/99
* Objetivo    : MUDAN‡A DE INDEXADOR DO CONTRATO 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
***************************************************************************
FUNCTION IND_IMOV()
SAVE SCREEN TO TELA03
PTIT( "MUDAN€A DE INDEXADOR DOS FINANCIAMENTOS")
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
USE CORRETOR INDEX CORRETOR
GOTO TOP
SELE 9
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
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = SPACE(5)
      @ 06,02 SAY ALLTRIM(sisGRUPO1)+" : "
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
      @ 07,02 SAY ALLTRIM(sisUNID1)+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
         PLQUA("01")
		 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  "+ALLTRIM(sisUNID1)+" Dispon¡veis neste Produto.")
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
               PMSGR( "Aten‡Æo !",3)
               PMENSAG ( ALLTRIM(sisUNID1)+" NÆo Vendido !")
               PPARA()
            ELSE
               ePARTE = 0
               F_INDICE(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,0)
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
                  PMENSAG ( ALLTRIM(sisUNID1)+" NÆo Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 1
                  F_INDICE(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,1)
               ENDIF
            ELSE
               IF ePROPR_2 = 1
                  PLQUA("01")
                  PMSGR( "Aten‡Æo !",3)
                  PMENSAG ( ALLTRIM(sisUNID1)+" NÆo Vendido !")
                  PPARA()
               ELSE
                  ePARTE = 2
                  F_INDICE(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,2)
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
      @ 06,02 SAY ALLTRIM(sisGRUPO2)+" : "
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
		 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  "+ALLTRIM(sisUNID2)+" Dispon¡veis neste Produto.")
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
         CAB[1]="Produto"
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
            PMENSAG ( ALLTRIM(sisUNID2)+" NÆo Vendido !")
            PPARA()
         ELSE
            F_INDICE(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
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
		 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  Im‡veis Dispon¡veis neste Empreendimento.")
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
            PMSGR( "Aten‡Æo !",3)
            PMENSAG("Imovel NÆo Vendido !")
            PPARA()
         ELSE
            F_INDICE(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
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
FUNCTION F_INDICE
PARAMETERS pT_PRO,pQUADRA,pUNIDADE,pM_UNID
eMSG = "MUDAN‡A DE INDEXADOR DO FINANCIAMENTO DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pM_UNID=0,"Proprietario Unico",IIF(pM_UNID=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9)
PLQUA("01")
PPERG ( "Confirma o "+ALLTRIM(sisUNID1)+" (S/N) ? ")
IF sisRESP = "N"
   RETURN 0
ENDIF
PLQUA("01")
SELE 6
GOTO TOP
SEEK STR(pT_PRO,5)+pQUADRA+pUNIDADE+STR(pM_UNID,1)
eNUM_CLIE = NUM_CLIE
eINDICE   = INDICE
eDATA_INI = ULT_CORREC
eCORRETOR = CORRETOR
ePERIODO  = PERIODO
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK eNUM_CLIE
@ 11,02 SAY "Nome :" GET CLIENTE
@ 13,02 SAY "Indice Atual  : " GET eINDICE
@ 13,40 SAY "Periodicidade : " GET ePERIODO
@ 15,02 SAY "Corretor Atual: " GET eCORRETOR PICT "@R 99999" 
CLEAR GETS
* Novo Indice
SELE 9
GOTO TOP
SEEK eINDICE
@ 17,02 SAY "Novo Indice   : "
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="XXXXXXXXXXX"
PIC[2]="XXXXXXXXXXX"
CAB[1]="    "
CAB[2]="    "
DBEDIT(17,18,17,31,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
eIND_NOVO = NOME
@ 17,35 say "Periodicidade   : "
@ 17,54 PROMPT "MENSAL   "
@ 17,54 PROMPT "ANUAL    "
@ 17,54 PROMPT "ANUAL12  "
@ 17,54 PROMPT "SEMESTRAL"
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
CLEAR GETS
@ 19,02 SAY "Data Inicio Corre‡Æo : " GET eDATA_INI
READ
* Novo Corretor
SELE 7
GOTO TOP
SEEK eCORRETOR
eCOR_NOVO = 0
@ 21,02 SAY "Novo Corretor : "
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@R 999"
PIC[2]="@S12"
CAB[1]="    "
CAB[2]="    "
DBEDIT(21,18,21,38,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
eCOR_NOVO = CODIGO
PLQUA("01")
PPERG ( "Confirma a Mudan‡a (S/N) ? ")
IF sisRESP = "S"
   SELE 6
   IF F_BLOQR()
      REPLACE INDICE     WITH eIND_NOVO,;
              ULT_CORREC WITH eDATA_INI,;
              CORRETOR   WITH eCOR_NOVO
      REPLACE PERIODO    WITH ePERIODO
      UNLOCK
   ENDIF
   PLQUA("01")
   PPERG ( "Deseja Congelar o Financiamento (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PPERG ( "Aten‡Æo, Esta opera‡Æo NÆo ‡ reversivel, Confirma (S/N) ? ")
      IF sisRESP = "S"
         SELE 8
         eIND_CLAS = STR(pT_PRO,5)
         eIND_QUAD = pQUADRA
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
         USE &eARQ INDEX &eIND2,&eIND1,&eIND3
         GOTO TOP
         SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID
         GOTO TOP
         DO PLQUA   WITH "01"
		 PMENSAG("AGUARDE ...")
         IF F_BLOQA()
            REPLACE ALL VLR_ORIGIN WITH VLR_PARC
            UNLOCK
         ENDIF
      ENDIF
   ENDIF
   F_LACESSO(sisUSUARIO,"ROTINAS ESPECIAIS","MUDANCA DE INDEXADOR : "+ALLTRIM(STR(pT_PRO,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pM_UNID,1))) 
   PLQUA("01")
   PMSGR( "Altera‡Æo Concluida !",3)
ENDIF
RETURN 0
*************************  fim  *********************************
