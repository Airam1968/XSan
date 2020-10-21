*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_DEVE.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE DEVEDORES DE PARCELAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_DEVE()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE DEVEDORES")
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
      eDATA_INI   = CTOD("")
      eDATA_FIM   = CTOD("")
      DO WHILE .T.
         @ 07,02 SAY "Periodo : " GET eDATA_INI
         @ 07,25 SAY "Ate : "     GET eDATA_FIM
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
      F_DEVE(eCLAS_PRO,eQUADRA,eDATA_INI,eDATA_FIM)
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
      eDATA_INI   = CTOD("")
      eDATA_FIM   = CTOD("")
      DO WHILE .T.
         @ 07,02 SAY "Periodo : " GET eDATA_INI
         @ 07,25 SAY "Ate : "     GET eDATA_FIM
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
      F_DEVE(eCLAS_PRO,eBLOCO,eDATA_INI,eDATA_FIM)
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
      eDATA_INI   = CTOD("")
      eDATA_FIM   = CTOD("")
      DO WHILE .T.
         @ 07,02 SAY "Periodo : " GET eDATA_INI
         @ 07,25 SAY "Ate : "     GET eDATA_FIM
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
      F_DEVE(eCLAS_PRO,eBLOCO,eDATA_INI,eDATA_FIM)
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_DEVE
PARAMETERS pCLASSE,pQUADRA,pDATA_INI,pDATA_FIM
eMSG = "CONSULTA DE PARCELAS A PAGAR"
PCENTRA( eMSG,9)
SELE 8
eIND_CLAS = STR(pCLASSE,5)
eIND_QUAD = pQUADRA
eCLASSE   = pCLASSE
eQUADRA   = pQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
USE &eARQ INDEX &eIND2,&eIND1,&eIND3
GOTO TOP
PLQUA("01")
eORIGEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Origem dos Pagamentos (C)arteira,(B)anco ou (A)mbas ? " GET eORIGEM PICT "@!X" VALID eORIGEM $("CBA")
READ
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Tipo de Parcelas (S)inal,(N)ormais,(I)ntermediarias ou (A)mbas ? " GET eTIPO PICT "@!X" VALID eTIPO $("SNIA")
READ
ePROTESTO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Selecionar T‡tulos ‡ (P)rotesto,(N)‡o Protesto ou (A)mbos ? " GET ePROTESTO PICT "@!X" VALID ePROTESTO $("PNA")
READ
PLQUA("01")
eORDEM = SPACE(1)
@ 23,00 SAY "Emiss‡o de Relat‡rio por Ordem de Vencimento ou Produto (V/P) ? " GET eORDEM PICT "@!X" VALID eORDEM $("VP")
READ
SELE 8
GOTO TOP
IF eORDEM = "V"
   USE &eARQ INDEX &eIND2,&eIND3,&eIND1
   GOTO TOP
ELSE
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   GOTO TOP
ENDIF
DO CASE
   CASE eTIPO = "A"
      IF eORIGEM = "A"
         DO CASE
            CASE ePROTESTO = "A"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL
            CASE ePROTESTO = "P"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO
            CASE ePROTESTO = "N"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO          
         ENDCASE
      ELSE 
         IF eORIGEM = "C"
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B"
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO          
            ENDCASE
         ELSE
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO          
            ENDCASE
         ENDIF
      ENDIF
   CASE eTIPO = "N"
      IF eORIGEM = "A"
         DO CASE
            CASE ePROTESTO = "A"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
               .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
            CASE ePROTESTO = "P"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
               .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
            CASE ePROTESTO = "N"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
               .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
         ENDCASE
      ELSE 
         IF eORIGEM = "C"
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
            ENDCASE
         ELSE
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R")
            ENDCASE
         ENDIF
      ENDIF
   CASE eTIPO = "I" 
      IF eORIGEM = "A"
         DO CASE
            CASE ePROTESTO = "A"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
               .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
            CASE ePROTESTO = "P"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
               .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
            CASE ePROTESTO = "N"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
               .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
         ENDCASE
      ELSE 
         IF eORIGEM = "C"
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
            ENDCASE
         ELSE
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "I")
            ENDCASE
         ENDIF
      ENDIF
   CASE eTIPO = "S" 
      IF eORIGEM = "A"
         DO CASE
            CASE ePROTESTO = "A"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL ;
               .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
            CASE ePROTESTO = "P"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
               .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
            CASE ePROTESTO = "N"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
               .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
         ENDCASE
      ELSE 
         IF eORIGEM = "C"
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
            ENDCASE
         ELSE
            DO CASE
               CASE ePROTESTO = "A"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
               CASE ePROTESTO = "P"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
               CASE ePROTESTO = "N"
                  SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("")) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                  .AND. (SUBSTR(TIPO_PARC,1,1) = "S")
            ENDCASE
         ENDIF
      ENDIF
ENDCASE
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Devedores Neste Periodo !")
   PPARA()
ELSE
   DECLARE CAMPO[16],PIC[16],CAB[16],LENS[16]
   AFIELDS(CAMPO,.T.,LENS)
   CAMPO[1] ="NUM_IMOVEL"
   CAMPO[2] ="NUM_PARC"
   CAMPO[3] ="TIPO_PARC"
   CAMPO[4] ="DATA_VENC"
   CAMPO[5] ="VLR_PARC"
   CAMPO[6] ="DATA_PGTO"
   CAMPO[7] ="VLR_PGTO"
   CAMPO[8] ="COD_BANCO"
   CAMPO[9] ="ORIGEM"
   CAMPO[10]="PROTESTO"
   CAMPO[11]="CONTABIL"
   CAMPO[12]="OBS"
   CAMPO[13]="MULTA"
   CAMPO[14]="COMISSAO"
   CAMPO[15]="PERCENTUAL" 
   CAMPO[16]="CONTA"
   LENS[1] =5
   LENS[2] =5
   LENS[3] =3
   LENS[4] =8
   LENS[5] =19
   LENS[6] =8
   LENS[7] =19
   LENS[8] =12
   LENS[9] =1
   LENS[10]=1
   LENS[11]=1
   LENS[12]=10
   LENS[13]=10
   LENS[14]=10
   LENS[15]=10
   LENS[16]=3
   PIC[1] ="@R99999"
   PIC[2] ="@R99999"
   PIC[3] ="XXX"
   PIC[4] ="99/99/99"
   PIC[5] ="@R 99,999,999,999.99"
   PIC[6] ="99/99/99"
   PIC[7] ="@R 99,999,999,999.99"
   PIC[8] ="@R 999999999999999"
   PIC[9] ="@!X"
   PIC[10]="L"
   PIC[11]="L"
   PIC[12]="XXXXXXXXXX"
   PIC[13]="@R 999.99"
   PIC[14]="@R 999.99"
   PIC[15]="@R 999.99"
   PIC[16]="@R 999"
   CAB[1] ="Unidade"
   CAB[2] ="Parcela"
   CAB[3] ="Tipo(N/I)"
   CAB[4] ="Venc."
   CAB[5] ="Parcela"
   CAB[6] ="Pago"
   CAB[7] ="Vlr. Pagamento"
   CAB[8] ="Cod. Banco"
   CAB[9] ="Origem"
   CAB[10]="Protesto"
   CAB[11]="Contabil"
   CAB[12]="Obs."
   CAB[13]="Multa"
   CAB[14]="Tx. perm."
   CAB[15]="Tx. Comis."
   CAB[16]="Cod.Conta"
   @ 10,00 TO 21,79 DOUBLE
   DBEDIT(11,01,21,78,CAMPO,"F_CONS",PIC,CAB,CHR(196),CHR(179),CHR(205))
   PLQUA("01")
   PPERG ( "Deseja Totaliza‡Æo (S/N) ? ")
   IF sisRESP = "S"
      PLIMPA()
	  PCENTRA( "TOTALIZA‡Æo DE DEVEDORES",9)
      eTOT_PARC = 0
      eTOT_TIT  = 0
      GOTO TOP
      PLQUA("01")
	  PMENSAG("Aguarde, Efetuando o processo de Totaliza‡Æo ...")
      DO WHILE .NOT. EOF()
         eTOT_PARC = eTOT_PARC + VLR_PARC
         eTOT_TIT  = eTOT_TIT + 1
         SKIP
      ENDDO
      @ 10,10 SAY "Total Devedores : " GET eTOT_PARC PICT "@R 999,999,999,999.99"
	  @ 12,10 SAY "N‡mero Parcelas : " GET eTOT_TIT  PICT "@R 999,999"
      CLEAR GETS
      PPARA()
   ENDIF
ENDIF
RETURN 0
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_CONS
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN >= 7
            IF F_BLOQR()
               SAVE SCREEN TO TELA04
               SET CURSOR ON
               FLD_NAME = CAMPO[DB_COLUMN]
               FLD_PIC  = PIC[DB_COLUMN]
               @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
               READ
               SET CURSOR OFF
               RESTORE SCREEN FROM TELA04
               UNLOCK
            ENDIF
         ENDIF
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************

