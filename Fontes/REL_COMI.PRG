*********************************************************************
* Sistema     : Gest�o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_COMI.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE COMISSAO EM ABERTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_COMI()
SAVE SCREEN TO TELA03
PTIT( "RELAT�RIO DE COMISSOES EM ABERTO")
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
      PPERG ( "Deseja Selecionar "+sisGRUPO2+" (S/N) ? ")
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
      PMSGR( "Datas Inv�lidas !",3)
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
@ 23,00 SAY "Emiss�o de Relatorio de Comiss�o (E)mpresa ou (C)orretor ? " GET eTIPO PICT "@!X" VALID eTIPO $("EC")
READ
PLQUA("01")
eMULT = SPACE(1)
@ 23,00 SAY "Utiliza Criterio de Multiplica��o (S/N) ? " GET eMULT PICT "@!X" VALID eMULT $("SN")
READ
eMULTIPLICA = 0
IF eMULT = "S"
   @ 23,00 SAY "Informe o numero de vezes a multiplicar : " GET eMULTIPLICA PICT "@R 9"
   READ
ENDIF
eCORRETOR = 0
IF eTIPO = "C" 
   PLQUA("01")
   @ 23,02 SAY "Corretor      : "
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
      PMSGR( "N�o H� Vendas a Contabilizar !",3)
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
      PMSGR( "N�o H� Vendas a Contabilizar !",3)
      CLOSE DATABASES
      RESTORE SCREEN FROM TELA03
      RETURN nil
   ENDIF
   PLQUA("01")
   PMSGR( "Aten��o !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados para Impress�o ...")
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
   PMSGR( "Aten��o !",3)
   PMENSAG("N�o H� Comissoes Neste Per�odo !")
   PPARA()
ELSE
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
   ENDIF     
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   eTOT_COMI = 0
   eTOT_PARC = 0
   eTOT_TIT  = 0
   SELE 6
   GOTO TOP
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN004"
      @ 03,10  SAY sisLETRAE+"RELATORIO COMISSOES EM ABERTO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      @ 04,00  SAY "   Produto     : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," ",+" - "+ALLTRIM(eDESC_PRO)+" -     Grupo    : "+ALLTRIM(eQUADRA))
      IF eTIPO = "E"
         @ 05,00  SAY "Comissao Geral - Empresa"
      ELSE
         @ 05,00  SAY "Corretor No. "+STR(eCORRETOR,5)+" - "+eCORR_NOME
      ENDIF
      @ 06,00  SAY REPLICATE("=",135)
      @ 07,02  SAY "No. IMOVEL"
      @ 07,15  SAY "PROPRIETARIO"
      @ 07,55  SAY "DATA VENC."
      @ 07,67  SAY "TIPO"
      @ 07,75  SAY "NUM.PARC."
      @ 07,87  SAY "TAXA" 
      @ 07,99  SAY "VLR. PARCELA"
      @ 07,115 SAY "VLR. COMISSAO" 
      @ 08,00 SAY REPLICATE("=",135)
      LIN = 9
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         SELE 6
         @ LIN,02  SAY ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = PARTE
         eNUM_CLIE   = NUM_CLIE
         eSUB_COMI   = 0
         eSUB_PARC   = 0
         eVLR_COMI   = 0
         SELE 5
         GOTO TOP
         SEEK eNUM_CLIE
         @ LIN,15  SAY SUBSTR(CLIENTE,1,44)
         SELE 8
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1,1)
         DO WHILE .T.
            @ LIN,55  SAY DATA_VENC
            @ LIN,69  SAY TIPO_PARC
            @ LIN,76  SAY ALLTRIM(NUM_PARC)
            @ LIN,87  SAY PERCENTUAL PICT "@R 999.99" 
            @ LIN,99  SAY VLR_PARC   PICT "@R 999,999,999.99"
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
            @ LIN,115 SAY eVLR_COMI  PICT "@R 999,999,999.99" 
            eSUB_COMI  = eSUB_COMI  + eVLR_COMI
            eSUB_PARC  = eSUB_PARC  + VLR_PARC
            eTOT_TIT   = eTOT_TIT + 1
            LIN = LIN + 1
            SKIP
            IF STR(eCLAS_PRO,5) = CLASSE .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE
               LOOP
            ELSE
*              @ LIN,40  SAY "SubTotal    : "
*              @ LIN,109 SAY eSUB_PARC PICT "@R 999,999,999.99" 
*              @ LIN,125 SAY eSUB_COMI PICT "@R 999,999,999.99" 
               eTOT_COMI  = eTOT_COMI  + eSUB_COMI
               eTOT_PARC  = eTOT_PARC  + eSUB_PARC
*              LIN = LIN + 2
               SELE 6
               SKIP
               EXIT
            ENDIF
         ENDDO
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
      IF EOF()
         LIN = LIN + 1
         @ LIN,00 SAY REPLICATE("-",135)
         LIN = LIN + 1
         @ LIN,40  SAY "Total Geral : "
         @ LIN,76  SAY eTOT_TIT  PICT "@R 999,999"         
         @ LIN,99  SAY eTOT_PARC PICT "@R 999,999,999.99" 
         @ LIN,115 SAY eTOT_COMI PICT "@R 999,999,999.99" 
         LIN = LIN + 2
      ENDIF
      @ 58,00 SAY REPLICATE("=",135)
      @ 59,104 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
