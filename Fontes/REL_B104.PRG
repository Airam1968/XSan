*********************************************************************
* Sistema     : Gest�o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_B104.PRG
* Data        : 01/01/99
* Objetivo    : EMISSAO DE BOLETOS PARA PAGAMENTO - CAIXA ECONOMICA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_B104()
SAVE SCREEN TO TELA03
PTIT( "EMISSAO DE BOLETOS - CAIXA ECON�MICA FEDERAL")
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Boleto (P)equeno, (M)edio ou (G)rande ? " GET eTIPO PICT "@!" VALID eTIPO $("PMG")
READ
SELE 1
USE STATUS
GOTO TOP
eEMPRESA = EMPRESA
eCIDADE  = CIDADE
eESTADO  = ESTADO
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
USE CONTAS INDEX CONTAS
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
   eCLAS_PRO  = CODIGO
   eTIPO_PRO  = TIPO
   eCOD_CONTA = COD_CONTA
   SELE 9 
   SEEK eCOD_CONTA
   eCOD_CONTA = ALLTRIM(COD_AGENCI)+"."+ALLTRIM(COD_CC)
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
         @ 06,02 SAY sisGRUPO1+"  : "
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
         PLQUA("01")
		 PPERG ( "Deseja Selecionar "+sisUNID1+" (S/N) ? ")
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID1+" : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
			   PMSGR( "Aten��o !",3)
			   PMENSAG("N�o H� "+sisUNID1+" Dispon�veis !")
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
               ePARTE = 0
            ELSE
               PLQUA("01")
               @ 23,00 SAY "Informe a Emissao para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
               READ
               IF eOPCAO = 1   
                  ePARTE = 1
               ELSE
                  ePARTE = 2
               ENDIF
            ENDIF
         ENDIF
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
         DBEDIT(6,08,6,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
         PLQUA("01")
         PPERG ( "Deseja Selecionar "+sisUNID2+" (S/N) ? ")
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID2+" : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
			   PMSGR( "Aten��o !",3)
			   PMENSAG("N�o H� "+sisUNID2+" Dispon�veis !")
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
               ePARTE = 0
            ELSE
               PLQUA("01")
               @ 23,00 SAY "Informe a Emissao para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
               READ
               IF eOPCAO = 1   
                  ePARTE = 1
               ELSE
                  ePARTE = 2
               ENDIF
            ENDIF
         ENDIF
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
PPERG ( "2a. Via de Boleto (S/N) ? ")
IF sisRESP = "N"
   eCOD_BANCO  = 0
   DO WHILE .T.
      @ 10,02 SAY "Codigo Inicial do Boleto : " GET eCOD_BANCO PICT "@R 999999999999" 
      READ
      IF eCOD_BANCO = 0
         PLQUA("01")
         PMSGR( "Codigo Invalido !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   eVIA2 = .F.
ELSE
   eVIA2 = .T.
   PLQUA("01") 
   PPERG ( "Deseja Mudar a Razao Social (S/N) ? ")
   IF sisRESP = "S"
      SAVE SCREEN
      @ 23,00 SAY "Empresa : " GET eEMPRESA
      READ
      RESTORE SCREEN
   ENDIF
ENDIF
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
   GOTO TOP
   IF eNUM_IMOVEL <> SPACE(5)
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
      GOTO TOP
   ELSE
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
      GOTO TOP
   ENDIF
   SET DEVICE TO SCREEN
   IF EOF()
      PLQUA("01")
      PMSGR( "Aten��o !",3)
	  PMENSAG("N�o H� Boletos a Serem Emitidos !")
      PPARA()
   ELSE
      PPERG ( "Emite Mensagens Automaticas nos Boletos (S/N) ? ")
      IF sisRESP = "S"
         eFAZ = .T.
      ENDIF
	  PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      SET DEVICE TO PRINT
      SELE 8
      GOTO TOP
      DO WHILE .NOT. EOF()
         LIN = PROW()
         @ LIN,00  SAY sisLETRAC+" "
         IF eTIPO = "P"
            LIN = LIN + 1
         ENDIF
         @ LIN,03  SAY eCIDADE+" - "+eESTADO
         @ LIN,90  SAY DATA_VENC
         LIN = LIN + 2
         IF eVIA2
            @ LIN,02  SAY eEMPRESA 
            @ LIN,90  SAY eCOD_CONTA
         ENDIF
         LIN = LIN + 1
         eNUM_IMOVEL = NUM_IMOVEL
         eQUADRA     = QUADRA
         ePARTE      = PARTE      
         SELE 6
         USE
         USE RENEGOCI INDEX RENEGOCI                            
         GOTO TOP
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE
         GOTO TOP
         IF EOF()
            SELE 6
            USE CAD_VEND INDEX CAD_VEND,DATA_VEN
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            eQUANT_PARC = PARCELAS
            eOBSERVACAO = ALLTRIM(OBSERVACAO)
         ELSE
            GOTO BOTTOM
            eQUANT_PARC = PARCELAS
            eOBSERVACAO = ALLTRIM(OBSERVACAO)
         ENDIF
         SELE 8
         @ LIN,02  SAY DTOC(DATE())
         eVAZIO  = "00"
         eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
         @ LIN,20  SAY eINDENT
         @ LIN,35  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eQUANT_PARC,5))
         @ LIN,55  SAY "DM"
         IF eVIA2
            @ LIN,65  SAY DTOC(DATE())
            @ LIN,90  SAY ALLTRIM(COD_BANCO)
         ENDIF
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = PARTE
         LIN = LIN + 2
         @ LIN,25  SAY "ESPEC."
         @ LIN,37  SAY ALLTRIM(sisMOEDA)
         IF eOBSERVACAO = "BOLETO20X"
            @ LIN,90  SAY VLR_PARC*1.25 PICT "@R 999,999,999.99"
         ELSE
            @ LIN,90  SAY VLR_PARC PICT "@R 999,999,999.99"
         ENDIF
         IF eFAZ
            IF eOBSERVACAO = "BOLETO20X"
               LIN = LIN + 3
               @ LIN,05 SAY "PAGAMENTO ATE O VENCIMENTO 20% DE DESCONTO"
               LIN = LIN + 1
               @ LIN,05 SAY "PAGAMENTO ATE 05 DO VENCIMENTO 18% DE DESCONTO"
               LIN = LIN + 1
               @ LIN,05 SAY "APOS O VENCIMENTO MULTA 2% + 1% A.M."
               LIN = LIN + 1
               @ LIN,05 SAY " "
               LIN = LIN + 1
               @ LIN,05 SAY " "
               IF eTIPO = "P"
                  LIN = LIN + 1
               ELSE
                  IF eTIPO = "M"
                     LIN = LIN + 2
                  ELSE
                     IF eTIPO = "G"
                        LIN = LIN + 3
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               LIN = LIN + 3
               SELE 9
               USE MSG_BOLE INDEX MSG_PROD,MSG_BOLE
               GOTO TOP
               SEEK eTIPO_PRO
               @ LIN,05 SAY LINHA_1
               LIN = LIN + 1
               @ LIN,05 SAY LINHA_2
               LIN = LIN + 1
               @ LIN,05 SAY LINHA_3
               LIN = LIN + 1
               @ LIN,05 SAY LINHA_4
               LIN = LIN + 1
               @ LIN,05 SAY LINHA_5
               IF eTIPO = "P"
                  LIN = LIN + 1
               ELSE
                  IF eTIPO = "M"
                     LIN = LIN + 2
                  ELSE
                     IF eTIPO = "G"
                        LIN = LIN + 3
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            SELE 8
         ELSE
            IF eTIPO = "P"
               LIN = LIN + 7
            ELSE
               IF eTIPO = "M"
                  LIN = LIN + 9
               ELSE
                  IF eTIPO = "G"
                     LIN = LIN + 10
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         SELE 6
         USE CAD_VEND INDEX CAD_VEND,DATA_VEN
         GOTO TOP
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         ePROPR = NUM_CLIE
         eOBSERVACAO = ALLTRIM(OBSERVACAO)         
         IF FOUND()
            SELE 5
            GOTO TOP
            SEEK ePROPR
            @ LIN,10  SAY CLIENTE
            @ LIN,65  SAY "C.P.F. : "+CPF_CGC
            LIN = LIN + 1
            IF OPCAO = "R"
               @ LIN,10  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
               LIN = LIN + 1
               @ LIN,10  SAY CEP_R PICT "@R 99999-999"
               @ LIN,20  SAY "- "+ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            ELSE
               @ LIN,10  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
               LIN = LIN + 1
               @ LIN,10  SAY CEP_C PICT "@R 99999-999"
               @ LIN,20  SAY "- "+ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            ENDIF
         ELSE
            @ LIN,10 SAY "CLIENTE N�o ENCONTRADO, CODIGO : "+STR(ePROPR,1,5)
            LIN = LIN + 2
         ENDIF
         LIN = LIN + 8
         @ LIN,00 SAY " "
         IF .NOT. eVIA2
            SELE 8
            SET DEVICE TO SCREEN 
            IF F_BLOQR()
               REPLACE COD_BANCO WITH ALLTRIM(STR(eCOD_BANCO,12))
               UNLOCK
            ENDIF
            eCOD_BANCO = eCOD_BANCO + 1
            @ 10,02
            @ 10,02 SAY "Codigo Atual do Boleto   : " GET eCOD_BANCO PICT "@R 999999999999" 
            CLEAR GETS
            SET DEVICE TO PRINT
         ENDIF
         SELE 8
         SKIP
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
   ENDIF
ELSE
   IF eTIPO_PRO = 1
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      SET DEVICE TO SCREEN
      PPERG ( "Emite Mensagens Automaticas nos Boletos (S/N) ? ")
      IF sisRESP = "S"
         eFAZ = .T.
      ENDIF
	  PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      SET DEVICE TO PRINT
      DO WHILE .NOT. EOF()
         eIND_CLAS = NUM_LOTEAM
         eIND_QUAD = NUM_QUADRA
         SELE 8
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         USE &eARQ INDEX &eIND1,&eIND2,&eIND3
         GOTO TOP
         SET FILTER TO CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
         GOTO TOP
         DO WHILE .NOT. EOF()
            LIN = PROW()
            @ LIN,00  SAY sisLETRAC+" "
            IF eTIPO = "P"
               LIN = LIN + 1
            ENDIF
            @ LIN,03  SAY eCIDADE+" - "+eESTADO
            @ LIN,90  SAY DATA_VENC
            LIN = LIN + 2
            IF eVIA2
               @ LIN,02  SAY eEMPRESA 
               @ LIN,90  SAY eCOD_CONTA
            ENDIF
            LIN = LIN + 1
            eNUM_IMOVEL = NUM_IMOVEL
            eQUADRA     = QUADRA
            ePARTE      = PARTE      
            SELE 6
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            IF FOUND()
               eQUANT_PARC = PARCELAS
               eOBSERVACAO = ALLTRIM(OBSERVACAO)
            ELSE
               eQUANT_PARC = 0
               eOBSERVACAO = SPACE(50)
            ENDIF
            SELE 8
            @ LIN,02  SAY DTOC(DATE())
            eVAZIO  = "00"
            eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
            @ LIN,20  SAY eINDENT
            @ LIN,35  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eQUANT_PARC,5))
            @ LIN,55  SAY "DM"
            IF eVIA2
               @ LIN,65  SAY DTOC(DATE())
               @ LIN,90  SAY ALLTRIM(COD_BANCO)
            ENDIF
            LIN = LIN + 2
            @ LIN,25  SAY "ESPEC."
            @ LIN,37  SAY ALLTRIM(sisMOEDA)
            IF eOBSERVACAO = "BOLETO20X"
               @ LIN,90  SAY VLR_PARC*1.25 PICT "@R 999,999,999.99"
            ELSE
               @ LIN,90  SAY VLR_PARC PICT "@R 999,999,999.99"
            ENDIF
            IF eFAZ
               IF eOBSERVACAO = "BOLETO20X"
                  LIN = LIN + 3
                  @ LIN,05 SAY "PAGAMENTO ATE O VENCIMENTO 20% DE DESCONTO"
                  LIN = LIN + 1
                  @ LIN,05 SAY "PAGAMENTO ATE 05 DO VENCIMENTO 18% DE DESCONTO"
                  LIN = LIN + 1
                  @ LIN,05 SAY "APOS O VENCIMENTO MULTA 2% + 1% A.M."
                  LIN = LIN + 1
                  @ LIN,05 SAY " "
                  LIN = LIN + 1
                  @ LIN,05 SAY " "
                  IF eTIPO = "P"
                     LIN = LIN + 1
                  ELSE
                     IF eTIPO = "M"
                        LIN = LIN + 2
                     ELSE
                        IF eTIPO = "G"
                           LIN = LIN + 3
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  LIN = LIN + 3
                  SELE 9
                  USE MSG_BOLE INDEX MSG_PROD,MSG_BOLE
                  GOTO TOP
                  SEEK eTIPO_PRO
                  @ LIN,05 SAY LINHA_1
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_2
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_3
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_4
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_5
                  IF eTIPO = "P"
                     LIN = LIN + 1
                  ELSE
                     IF eTIPO = "M"
                        LIN = LIN + 2
                     ELSE
                        IF eTIPO = "G"
                           LIN = LIN + 3
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               SELE 8
            ELSE
               IF eTIPO = "P"
                  LIN = LIN + 7
               ELSE
                  IF eTIPO = "M"
                     LIN = LIN + 9
                  ELSE
                     IF eTIPO = "G"
                        LIN = LIN + 10
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            SELE 6
            USE CAD_VEND INDEX CAD_VEND,DATA_VEN
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            ePROPR = NUM_CLIE
            eOBSERVACAO = ALLTRIM(OBSERVACAO)
            SELE 5
            GOTO TOP
            SEEK ePROPR
            @ LIN,10  SAY CLIENTE
            @ LIN,65  SAY "C.P.F. : "+CPF_CGC
            LIN = LIN + 1
            IF OPCAO = "R"
               @ LIN,10  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
               LIN = LIN + 1
               @ LIN,10  SAY CEP_R PICT "@R 99999-999"
               @ LIN,20  SAY "- "+ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            ELSE
               @ LIN,10  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
               LIN = LIN + 1
               @ LIN,10  SAY CEP_C PICT "@R 99999-999"
               @ LIN,20  SAY "- "+ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            ENDIF
            LIN = LIN + 8
            @ LIN,00 SAY " "
            IF .NOT. eVIA2
               SELE 8
               SET DEVICE TO SCREEN 
               IF F_BLOQR()
                  REPLACE COD_BANCO WITH ALLTRIM(STR(eCOD_BANCO,12))
                  UNLOCK
               ENDIF
               eCOD_BANCO = eCOD_BANCO + 1
               @ 10,02
               @ 10,02 SAY "Codigo Atual do Boleto   : " GET eCOD_BANCO PICT "@R 999999999999" 
               CLEAR GETS
               SET DEVICE TO PRINT
            ENDIF
            SELE 8
            SKIP
            DO PTECLA
            IF sisINTERR
               SET DEVICE TO SCREEN
               CLOSE ALL
               RETURN nil
            ENDIF
         ENDDO
         SELE 3
         SKIP
      ENDDO
   ELSE
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      SET DEVICE TO SCREEN
      PPERG ( "Emite Mensagens Automaticas nos Boletos (S/N) ? ")
      IF sisRESP = "S"
         eFAZ = .T.
      ENDIF
	  PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      SET DEVICE TO PRINT
      DO WHILE .NOT. EOF()
         eIND_CLAS = NUM_LOTEAM
         eIND_QUAD = NUM_BLOCO
         SELE 8
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         USE &eARQ INDEX &eIND1,&eIND2,&eIND3
         GOTO TOP
         SET FILTER TO CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0) .AND. ORIGEM <> "B"
         GOTO TOP
         DO WHILE .NOT. EOF()
            LIN = PROW()
            @ LIN,00  SAY sisLETRAC+" "
            IF eTIPO = "P"
               LIN = LIN + 1
            ENDIF
            @ LIN,03  SAY eCIDADE+" - "+eESTADO
            @ LIN,90  SAY DATA_VENC
            LIN = LIN + 2
            IF eVIA2
               @ LIN,02  SAY eEMPRESA 
               @ LIN,90  SAY eCOD_CONTA
            ENDIF
            LIN = LIN + 1
            eNUM_IMOVEL = NUM_IMOVEL
            eQUADRA     = QUADRA
            ePARTE      = PARTE      
            SELE 6
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            IF FOUND()
               eQUANT_PARC = PARCELAS
               eOBSERVACAO = ALLTRIM(OBSERVACAO)
            ELSE
               eQUANT_PARC = 0
               eOBSERVACAO = SPACE(50)
            ENDIF
            SELE 8
            @ LIN,02  SAY DTOC(DATE())
            eVAZIO  = "00"
            eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
            @ LIN,20  SAY eINDENT
            @ LIN,35  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eQUANT_PARC,5))
            @ LIN,55  SAY "DM"
            IF eVIA2
               @ LIN,65  SAY DTOC(DATE())
               @ LIN,90  SAY ALLTRIM(COD_BANCO)
            ENDIF
            LIN = LIN + 2
            @ LIN,25  SAY "ESPEC."
            @ LIN,37  SAY ALLTRIM(sisMOEDA)
            IF eOBSERVACAO = "BOLETO20X"
               @ LIN,90  SAY VLR_PARC*1.25 PICT "@R 999,999,999.99"
            ELSE
               @ LIN,90  SAY VLR_PARC PICT "@R 999,999,999.99"
            ENDIF
            IF eFAZ
               IF eOBSERVACAO = "BOLETO20X"
                  LIN = LIN + 3
                  @ LIN,05 SAY "PAGAMENTO ATE O VENCIMENTO 20% DE DESCONTO"
                  LIN = LIN + 1
                  @ LIN,05 SAY "PAGAMENTO ATE 05 DO VENCIMENTO 18% DE DESCONTO"
                  LIN = LIN + 1
                  @ LIN,05 SAY "APOS O VENCIMENTO MULTA 2% + 1% A.M."
                  LIN = LIN + 1
                  @ LIN,05 SAY " "
                  LIN = LIN + 1
                  @ LIN,05 SAY " "
                  IF eTIPO = "P"
                     LIN = LIN + 1
                  ELSE
                     IF eTIPO = "M"
                        LIN = LIN + 2
                     ELSE
                        IF eTIPO = "G"
                           LIN = LIN + 3
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  LIN = LIN + 3
                  SELE 9
                  USE MSG_BOLE INDEX MSG_PROD,MSG_BOLE
                  GOTO TOP
                  SEEK eTIPO_PRO
                  @ LIN,05 SAY LINHA_1
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_2
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_3
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_4
                  LIN = LIN + 1
                  @ LIN,05 SAY LINHA_5
                  IF eTIPO = "P"
                     LIN = LIN + 1
                  ELSE
                     IF eTIPO = "M"
                        LIN = LIN + 2
                     ELSE
                        IF eTIPO = "G"
                           LIN = LIN + 3
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               SELE 8
            ELSE
               IF eTIPO = "P"
                  LIN = LIN + 7
               ELSE
                  IF eTIPO = "M"
                     LIN = LIN + 9
                  ELSE
                     IF eTIPO = "G"
                        LIN = LIN + 10
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            SELE 6
            IF ePARTE = 0 .OR. ePARTE = 1
               ePROPR_1 = NUM_CLIE
               SELE 5
               GOTO TOP
               SEEK ePROPR_1
            ELSE
               ePROPR_2 = NUM_CLIE
               SELE 5
               GOTO TOP
               SEEK ePROPR_2
            ENDIF
            @ LIN,10  SAY CLIENTE
            @ LIN,65  SAY "C.P.F. : "+CPF_CGC
            LIN = LIN + 1
            IF OPCAO = "R"
               @ LIN,10  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
               LIN = LIN + 1
               @ LIN,10  SAY CEP_R PICT "@R 99999-999"
               @ LIN,20  SAY "- "+ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            ELSE
               @ LIN,10  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
               LIN = LIN + 1
               @ LIN,10  SAY CEP_C PICT "@R 99999-999"
               @ LIN,20  SAY "- "+ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            ENDIF
            LIN = LIN + 9
            @ LIN,00 SAY " "
            IF .NOT. eVIA2
               SELE 8
               SET DEVICE TO SCREEN             
               IF F_BLOQR()
                  REPLACE COD_BANCO WITH ALLTRIM(STR(eCOD_BANCO,12))
                  UNLOCK
               ENDIF
               eCOD_BANCO = eCOD_BANCO + 1
               @ 10,02
               @ 10,02 SAY "Codigo Atual do Boleto   : " GET eCOD_BANCO PICT "@R 999999999999" 
               CLEAR GETS
               SET DEVICE TO PRINT
            ENDIF
            SELE 8
            SKIP
            DO PTECLA
            IF sisINTERR
               SET DEVICE TO SCREEN
               CLOSE ALL
               RETURN nil
            ENDIF
         ENDDO
         SELE 4
         SKIP
      ENDDO
   ENDIF
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
