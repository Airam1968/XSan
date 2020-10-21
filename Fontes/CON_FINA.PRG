*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_FINA.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE STATUS DOS FINANCIAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_FINA()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE STATUS DOS FINANCIAMENTOS")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
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
         DBEDIT(6,14,6,45,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
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
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
      ENDIF
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar o Bloco (S/N) ? ")
      IF sisRESP = "S"
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
         DBEDIT(6,10,6,41,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("01/01/1980")
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
IF eQUADRA <> SPACE(5)
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. ((DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .OR. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)) .AND. ATIVA
   GOTO TOP
ELSE
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados ...")
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
         APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .OR. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)) .AND. ATIVA
         GOTO TOP
         SELE 3
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
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
         APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .OR. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)) .AND. ATIVA
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
      SET EXCLUSIVE OFF
   ENDIF
ENDIF
SELE 8
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Financiamentos Neste Per¡odo !")
   PPARA()
ELSE
   eTOT_CFINA  = 0
   eTOT_CPAGO  = 0
   eTOT_CDEVE  = 0
   eTOT_TTITU  = 0
   eTOT_TPAGO  = 0
   eTOT_TDEVE  = 0
   eTOT_TCOB1  = 0
   eTOT_TCOB2  = 0
   eTOT_TCAR1  = 0
   eTOT_TCAR2  = 0
   eTOT_TPROT  = 0
   eTOT_TITU   = 0
   eTOT_PAGO   = 0
   eTOT_DEVE   = 0
   eTOT_COB1   = 0
   eTOT_COB2   = 0
   eTOT_CAR1   = 0
   eTOT_CAR2   = 0
   eTOT_PROT   = 0
   DO WHILE .NOT. EOF()
      SELE 8
      eCLASSE     = CLASSE
      eQUADRA     = QUADRA
      eNUM_IMOVEL = NUM_IMOVEL
      ePARTE      = PARTE
      * Valores Pagos e Devidos
      IF DATA_PGTO <> CTOD("")
         eTOT_TPAGO  = eTOT_TPAGO + 1
         eTOT_PAGO   = eTOT_PAGO  + VLR_PARC
      ELSE
         eTOT_TDEVE  = eTOT_TDEVE + 1
         eTOT_DEVE   = eTOT_DEVE  + VLR_PARC
      ENDIF
      * Valores em Carteira ou Cobranca
      IF ORIGEM = "B"
         IF DATA_PGTO <> CTOD("")
            eTOT_TCAR1 = eTOT_TCAR1 + 1
            eTOT_CAR1  = eTOT_CAR1 + VLR_PARC
         ELSE
            eTOT_TCAR2 = eTOT_TCAR2 + 1
            eTOT_CAR2  = eTOT_CAR2 + VLR_PARC
         ENDIF
      ELSE
         IF DATA_PGTO <> CTOD("")
            eTOT_TCOB1 = eTOT_TCOB1 + 1
            eTOT_COB1  = eTOT_COB1 + VLR_PARC
         ELSE
            eTOT_TCOB2 = eTOT_TCOB2 + 1
            eTOT_COB2  = eTOT_COB2 + VLR_PARC
         ENDIF
      ENDIF
      * Valores em Protesto
      IF PROTESTO
         eTOT_TPROT = eTOT_TPROT + 1
         eTOT_PROT  = eTOT_PROT + VLR_PARC
      ENDIF
      * Valores Totais da Carteira
      eTOT_TTITU = eTOT_TTITU + 1
      eTOT_TITU  = eTOT_TITU + VLR_PARC
      SKIP
      IF eCLASSE <> CLASSE .AND. eQUADRA <> QUADRA .OR. eNUM_IMOVEL <> NUM_IMOVEL .OR. PARTE <> ePARTE
         eTOT_CFINA = eTOT_CFINA + 1
      ENDIF
   ENDDO    
*  @  10,10 SAY "Total de Financ. : "
*  @  10,30 SAY eTOT_CFINA     PICT "@R 99,999"
   @  12,10 SAY "Total de Titulos : "
   @  12,30 SAY eTOT_TTITU     PICT "@R 99,999"
   @  12,40 SAY eTOT_TITU      PICT "@R 999,999,999,999.99"
   @  13,10 SAY "Tit.Pagos        : "
   @  13,30 SAY eTOT_TPAGO     PICT "@R 99,999"
   @  13,40 SAY eTOT_PAGO      PICT "@R 999,999,999,999.99"
   @  14,10 SAY "Titulos Devidos  : "
   @  14,30 SAY eTOT_TDEVE     PICT "@R 99,999"
   @  14,40 SAY eTOT_DEVE      PICT "@R 999,999,999,999.99"
   @  15,10 SAY "Tit.Carteira Pago: "
   @  15,30 SAY eTOT_TCAR1     PICT "@R 99,999"
   @  15,40 SAY eTOT_CAR1      PICT "@R 999,999,999,999.99"
   @  16,10 SAY "Tit.Carteira Aber: "
   @  16,30 SAY eTOT_TCAR2     PICT "@R 99,999"
   @  16,40 SAY eTOT_CAR2      PICT "@R 999,999,999,999.99"
   @  17,10 SAY "Tit.Cobran‡a Pago: "
   @  17,30 SAY eTOT_TCOB1     PICT "@R 99,999"
   @  17,40 SAY eTOT_COB1      PICT "@R 999,999,999,999.99"
   @  18,10 SAY "Tit.Cobranca Aber: "
   @  18,30 SAY eTOT_TCOB2     PICT "@R 99,999"
   @  18,40 SAY eTOT_COB2      PICT "@R 999,999,999,999.99"
   @  19,10 SAY "Tit. em Protesto : "
   @  19,30 SAY eTOT_TPROT     PICT "@R 99,999"
   @  19,40 SAY eTOT_PROT      PICT "@R 999,999,999,999.99"
   CLEAR GETS
   PMENSAG("Pressione Qualquer Tecla Para Terminar ...")
   PPARA() 
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
