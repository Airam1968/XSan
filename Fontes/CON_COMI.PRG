*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_COMI.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE COMISSAO EM ABERTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_COMI()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA TOTAIS DE COMISSOES EM ABERTO")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
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
      PPERG ( "Deseja Selecionar o "+sisGRUPO2+" (S/N) ? ")
      IF sisRESP = "S"
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
         DBEDIT(6,08,6,76,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
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
      PPERG ( "Deseja Selecionar o "+sisGRUPO1+" (S/N) ? ")
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
eDATA_INI   = CTOD("")
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
PLQUA("01")
eTIPO = SPACE(1)
@ 23,00 SAY "Emiss‡o de Relat‡rio de Comissao (E)mpresa ou (U)nidade  ? " GET eTIPO PICT "@!X" VALID eTIPO $("EU")
READ
PLQUA("01")
eMULT = SPACE(1)
@ 23,00 SAY "Utiliza Criterio de Multiplica‡Æo (S/N) ? " GET eMULT PICT "@!X" VALID eMULT $("SN")
READ
eMULTIPLICA = 0
IF eMULT = "S"
   @ 23,00 SAY "Informe o numero de vezes a multiplicar : " GET eMULTIPLICA PICT "@R 9"
   READ
ENDIF
eCORRETOR = 0
IF eTIPO = "U" 
   PLQUA("01")
   @ 23,02 SAY "Unidade       : "
   SELE 7
   GO TOP
   DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R 999"
   PIC[2]="@S12"
   CAB[1]="    "
   CAB[2]="    "
   DBEDIT(23,18,23,38,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCORRETOR  = CODIGO
   eCORR_NOME = NOME
ENDIF
IF eQUADRA <> SPACE(5)
   SELE 6
   GOTO TOP
   IF eTIPO = "E"
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA    
      GOTO TOP
   ELSE
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. CORRETOR = eCORRETOR
      GOTO TOP
   ENDIF
   IF EOF()
      PLQUA("01")
      PMSGR( "NÆo H  Vendas a Contabilizar !",3)
      CLOSE DATABASES
      RESTORE SCREEN FROM TELA03
      RETURN nil
   ENDIF
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" 
   GOTO TOP
ELSE
   SELE 6
   GOTO TOP
   IF eTIPO = "E"
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5)     
      GOTO TOP
   ELSE
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. CORRETOR = eCORRETOR
      GOTO TOP
   ENDIF
   IF EOF()
      PLQUA("01")
      PMSGR( "NÆo H  Vendas a Contabilizar !",3)
      CLOSE DATABASES
      RESTORE SCREEN FROM TELA03
      RETURN nil
   ENDIF
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
         APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" 
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
         APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. SUBSTR(TIPO_PARC,1,1) = "S" 
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
   PMSGR( "Atenc‡o !",3)
   PMENSAG("NÆo H  Comiss‡es Neste Per¡odo !")
   PPARA()
ELSE
   PMENSAG("Aguarde o Processamento ...")
   eTOT_COMI = 0
   eTOT_PARC = 0
   eTOT_TIT  = 0
   SELE 6
   GOTO TOP
   DO WHILE .NOT. EOF()
      IF eTIPO = "E"
         @ 10,00  SAY "Comissao Geral - Empresa"
      ELSE
         @ 10,00  SAY "Unidade  No. "+STR(eCORRETOR,5)+" - "+eCORR_NOME
      ENDIF
      SELE 6
      eQUADRA     = QUADRA
      eNUM_IMOVEL = NUM_IMOVEL
      ePARTE      = PARTE
      eNUM_CLIE   = NUM_CLIE
      eSUB_COMI   = 0
      eSUB_PARC   = 0
      eVLR_COMI   = 0
      SELE 8
      SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1,1)
      DO WHILE .T.
         eVLR_COMI = 0
         IF eTIPO = "C"
            eVLR_COMI = VLR_PARC * (PERCENTUAL/100)
         ELSE
            IF eMULT = "S"
               eVLR_COMI = VLR_PARC * ((PERCENTUAL*eMULTIPLICA)/100)
            ELSE
               eVLR_COMI = VLR_PARC * (PERCENTUAL/100)
            ENDIF
         ENDIF
         eSUB_COMI  = eSUB_COMI  + eVLR_COMI
         eSUB_PARC  = eSUB_PARC  + VLR_PARC
         IF VLR_PARC <> 0
            eTOT_TIT   = eTOT_TIT + 1
         ENDIF
         SKIP
         IF STR(eCLAS_PRO,5) = CLASSE .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE
            LOOP
         ELSE
            @ 12,00  SAY "SubTotais   : "
            @ 14,00  SAY "Titulos     : " GET eTOT_TIT  PICT "@R 999,999,999.99"
            @ 16,00  SAY "Parcelas    : " GET eSUB_PARC PICT "@R 999,999,999.99"
            @ 18,00  SAY "Comiss‡o    : " GET eSUB_COMI PICT "@R 999,999,999.99"
            CLEAR GETS
            eTOT_COMI  = eTOT_COMI  + eSUB_COMI
            eTOT_PARC  = eTOT_PARC  + eSUB_PARC
            SELE 6
            SKIP
            EXIT
         ENDIF
      ENDDO
      IF EOF()
         @ 12,00  SAY "Total Geral : "
         @ 14,00  SAY "Titulos     : " GET eTOT_TIT  PICT "@R 999,999,999.99"
         @ 16,00  SAY "Parcelas    : " GET eTOT_PARC PICT "@R 999,999,999.99"
         @ 18,00  SAY "Comiss‡o    : " GET eTOT_COMI PICT "@R 999,999,999.99"
         CLEAR GETS
         PPARA()
      ENDIF
   ENDDO
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
