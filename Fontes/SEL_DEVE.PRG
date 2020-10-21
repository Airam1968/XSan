*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : SEL_DEVE.PRG
* Data        : 01/01/99
* Objetivo    : SELECAO DE DEVEDORES 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION SEL_DEVE()
SAVE SCREEN TO TELA03
PTIT("SELECAO DE DEVEDORES")
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
      * Consulta de Produto (Loteamento)
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
   CASE eTIPO_PRO = 2
      * Consulta de Grupo (Apartamentos)
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
   CASE eTIPO_PRO = 3
      * Consulta de Grupo 2 (Salas Comerciais)
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      PLQUA("01")
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("01/01/1900")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Limite Inadimpláncia : " GET eDATA_FIM
   READ
   IF eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Data Invalida !",3)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
PLQUA("01")
eTIPO_SEL = SPACE(1)
eNUM_INI = 0
eNUM_FIM = 0
@ 23,00 SAY "Seleá∆o Total ou Por Námero de Parcelas (T/N) ? " GET eTIPO_SEL PICT "@!X" VALID eTIPO_SEL $("TN")
READ
IF eTIPO_SEL = "N"
   DO WHILE .T.
      PLQUA("01")
      @ 23,00 SAY "Informe o Intervalo de Parcelas de : " GET eNUM_INI PICT "@R 999"
      @ 23,43 SAY "atá : " GET eNUM_FIM PICT "@R 999"
      READ
      IF eNUM_INI <> 0 .AND. eNUM_INI <> 0
         IF eNUM_FIM < eNUM_INI
            PLQUA("01")
            PMSGR("No. Final Menor que No. Inicial",3)
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
ENDIF
PLQUA("01")
PMSGR( "Atená∆o !",3)
PMENSAG("Aguarde a montagem do Banco de Dados ...")
IF eTIPO_PRO = 1
   SELE 5
   SET EXCLUSIVE ON
   USE CONTATO INDEX NUM_CLIE,CONTATO
   GOTO TOP
   IF F_BLOQA()
      REPLACE ALL SELECAO WITH " "
      UNLOCK
   ENDIF
   SET EXCLUSIVE OFF
   SELE 3
   SET FILTER TO STR(eCLAS_PRO,5) = NUM_LOTEAM .AND. CADASTRO 
   GOTO TOP
   DO WHILE .NOT. EOF()
      eIND_CLAS = STR(eCLAS_PRO,5)
      eIND_QUAD = NUM_QUADRA
      SELE 8
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
      eIND  = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
      USE &eARQ INDEX &eIND
      GOTO TOP
      eATIVA = .T.
      SET FILTER TO (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. DATA_PGTO = CTOD("") .AND. ATIVA = eATIVA
      GOTO TOP
      eTOT_PARC = 0
      eCLAS_VEN = CLASSE
      eQUAD_VEN = QUADRA
      eIMOV_VEN = NUM_IMOVEL
      ePART_VEN = STR(PARTE,1)
      eVAZIO  = "00"
      eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eCLAS_VEN))))+ALLTRIM(eCLAS_VEN)+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eQUAD_VEN))))+ALLTRIM(eQUAD_VEN)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIMOV_VEN))))+ALLTRIM(eIMOV_VEN)+ALLTRIM(ePART_VEN)
      DO WHILE .NOT. EOF()
         SELE 8
         DO WHILE (.NOT. EOF()) .AND. (eCLAS_VEN = CLASSE .AND. eQUAD_VEN = QUADRA .AND. eIMOV_VEN = NUM_IMOVEL .AND. ePART_VEN = STR(PARTE,1))
            PLQUA("01")
            eTOT_PARC = eTOT_PARC + 1            
			PMENSAG("Processando : "+ALLTRIM(eCLAS_VEN)+"/"+ALLTRIM(eQUAD_VEN)+"/"+ALLTRIM(eIMOV_VEN)+"/"+ALLTRIM(ePART_VEN)+" -  "+ALLTRIM(STR(eTOT_PARC,3)))
            SKIP
         ENDDO
         SELE 6
         GOTO TOP
         SEEK eCLAS_VEN+eQUAD_VEN+eIMOV_VEN+ePART_VEN
         eNUM_CLIE = NUM_CLIE
         SELE 5
         SEEK eNUM_CLIE
         IF eTIPO_SEL = "T"
            IF F_BLOQR()
               REPLACE SELECAO WITH "X",;
                       IDENTIF WITH eINDENT
               UNLOCK
            ENDIF
         ELSE
            IF eTOT_PARC >= eNUM_INI .AND. eTOT_PARC <= eNUM_FIM
               IF F_BLOQR()
                  REPLACE SELECAO WITH "X",;
                          IDENTIF WITH eINDENT
                  UNLOCK
               ENDIF
            ENDIF
         ENDIF
         eTOT_PARC = 0
         SELE 8
         eCLAS_VEN = CLASSE
         eQUAD_VEN = QUADRA
         eIMOV_VEN = NUM_IMOVEL
         ePART_VEN = STR(PARTE,1)
         eVAZIO  = "00"
         eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eCLAS_VEN))))+ALLTRIM(eCLAS_VEN)+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eQUAD_VEN))))+ALLTRIM(eQUAD_VEN)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIMOV_VEN))))+ALLTRIM(eIMOV_VEN)+ALLTRIM(ePART_VEN)
      ENDDO
      SELE 3
      SKIP
   ENDDO
ELSE
   SELE 5
   SET EXCLUSIVE ON
   USE CONTATO INDEX NUM_CLIE,CONTATO
   GOTO TOP
   IF F_BLOQA()
      REPLACE ALL SELECAO WITH " "
      UNLOCK
   ENDIF
   SET EXCLUSIVE OFF
   SELE 4
   SET FILTER TO STR(eCLAS_PRO,5) = NUM_LOTEAM .AND. CADASTRO 
   GOTO TOP
   DO WHILE .NOT. EOF()
      eIND_CLAS = STR(eCLAS_PRO,5)
      eIND_QUAD = NUM_BLOCO
      SELE 8
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
      eIND  = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
      USE &eARQ INDEX &eIND
      GOTO TOP
      SET FILTER TO DATA_VENC <= eDATA_FIM .AND. DATA_PGTO = CTOD("") .AND. ATIVA
      GOTO TOP
      eTOT_PARC = 0
      eCLAS_VEN = CLASSE
      eQUAD_VEN = QUADRA
      eIMOV_VEN = NUM_IMOVEL
      ePART_VEN = STR(PARTE,1)
      eVAZIO  = "00"
      eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eCLAS_VEN))))+ALLTRIM(eCLAS_VEN)+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eQUAD_VEN))))+ALLTRIM(eQUAD_VEN)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIMOV_VEN))))+ALLTRIM(eIMOV_VEN)+ALLTRIM(ePART_VEN)
      DO WHILE .NOT. EOF()
         SELE 8
         DO WHILE (.NOT. EOF()) .AND. (eCLAS_VEN = CLASSE .AND. eQUAD_VEN = QUADRA .AND. eIMOV_VEN = NUM_IMOVEL .AND. ePART_VEN = STR(PARTE,1))
            PLQUA("01")
            eTOT_PARC = eTOT_PARC + 1            
			PMENSAG("Processando : "+ALLTRIM(eCLAS_VEN)+"/"+ALLTRIM(eQUAD_VEN)+"/"+ALLTRIM(eIMOV_VEN)+"/"+ALLTRIM(ePART_VEN)+" -  "+ALLTRIM(STR(eTOT_PARC,3)))
            SKIP
         ENDDO
         SELE 6
         GOTO TOP
         SEEK eCLAS_VEN+eQUAD_VEN+eIMOV_VEN+ePART_VEN
         eNUM_CLIE = NUM_CLIE
         SELE 5
         SEEK eNUM_CLIE
         IF eTIPO_SEL = "T"
            IF F_BLOQR()
               REPLACE SELECAO WITH "X",;
                       IDENTIF WITH eINDENT
               UNLOCK
            ENDIF
         ELSE
            IF eTOT_PARC >= eNUM_INI .AND. eTOT_PARC <= eNUM_FIM
               IF F_BLOQR()
                  REPLACE SELECAO WITH "X",;
                          IDENTIF WITH eINDENT
                  UNLOCK
               ENDIF
            ENDIF
         ENDIF
         eTOT_PARC = 0
         SELE 8
         eCLAS_VEN = CLASSE
         eQUAD_VEN = QUADRA
         eIMOV_VEN = NUM_IMOVEL
         ePART_VEN = STR(PARTE,1)
         eVAZIO  = "00"
         eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eCLAS_VEN))))+ALLTRIM(eCLAS_VEN)+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(eQUAD_VEN))))+ALLTRIM(eQUAD_VEN)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIMOV_VEN))))+ALLTRIM(eIMOV_VEN)+ALLTRIM(ePART_VEN)
      ENDDO
      SELE 4
      SKIP
   ENDDO
ENDIF
PLQUA("01")
PMSGR("Seleá∆o Concluáda !",3)
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
