*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_BOLE.PRG
* Data        : 01/01/99
* Objetivo    : EMISSAO DE BOLETOS PARA PAGAMENTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_BOLE()
SAVE SCREEN TO TELA03
PTIT( "EMISSAO DE BOLETOS")
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
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eQUADRA  = SPACE(1)
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
               PMSGR( "Atená∆o !",3)
               PMENSAG("N∆o H† "+sisUNID1+" Disponá√≠veis "!")
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
               PMSGR( "Atená∆o !",3)
               PMENSAG("N∆o H† "+sisUNID2+" Disponá√≠veis "!")
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
      PMSGR( "Datas Inv†lidas !",3)
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
   GOTO TOP
   IF eNUM_IMOVEL <> SPACE(5)
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0)
      GOTO TOP
   ELSE
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0)
      GOTO TOP
   ENDIF
   SET DEVICE TO SCREEN
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† Boletos a Serem Emitidos !")
      PPARA()
   ELSE
      PPERG ( "Emite Mensagens nos Boletos (S/N) ? ")
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
         @ LIN,02  SAY sisLETRAC+" "
         LIN = LIN + 1
         @ LIN,02  SAY "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO."
         @ LIN,02  SAY "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO." 
         @ LIN,90  SAY DATA_VENC
         LIN = LIN + 4
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
         ELSE
            GOTO BOTTOM
            eQUANT_PARC = PARCELAS
         ENDIF
         SELE 8
         @ LIN,02  SAY DTOC(DATE())
         eVAZIO  = "00"
         eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
         @ LIN,20  SAY eINDENT
         @ LIN,35  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eQUANT_PARC,5))
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = PARTE
         LIN = LIN + 1
         @ LIN,25  SAY "132"
         @ LIN,37  SAY ALLTRIM(sisMOEDA)
         @ LIN,72  SAY ALLTRIM(sisMOEDA)
         @ LIN,90  SAY VLR_PARC PICT "@R 999,999,999.99"
         IF eFAZ
            LIN = LIN + 3
            SELE 9
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
            LIN = LIN + 2
            SELE 8
         ELSE
            LIN = LIN + 9
         ENDIF
         SELE 6
         USE CAD_VEND INDEX CAD_VEND,DATA_VEN
         GOTO TOP
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         ePROPR = NUM_CLIE
         IF FOUND()
            SELE 5
            GOTO TOP
            SEEK ePROPR
            @ LIN,05  SAY CLIENTE
            LIN = LIN + 1
            IF OPCAO = "R"
               @ LIN,05  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
               LIN = LIN + 1
               @ LIN,05  SAY CEP_R PICT "@R 99999-999"
               @ LIN,15  SAY "- "+ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            ELSE
               @ LIN,05  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
               LIN = LIN + 1
               @ LIN,05  SAY CEP_C PICT "@R 99999-999"
               @ LIN,15  SAY "- "+ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            ENDIF
         ELSE
            @ LIN,05 SAY "CLIENTE N∆o ENCONTRADO, CODIGO : "+STR(ePROPR,1,5)
            LIN = LIN + 2
         ENDIF
         LIN = LIN + 7
         @ LIN,00 SAY " "
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
      PPERG ( "Emite Mensagens nos Boletos (S/N) ? ")
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
         SET FILTER TO CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0)
         GOTO TOP
         DO WHILE .NOT. EOF()
            LIN = PROW()
            @ LIN,02  SAY sisLETRAC+" "
            LIN = LIN + 1
            @ LIN,02  SAY "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO."
            @ LIN,02  SAY "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO." 
            @ LIN,90  SAY DATA_VENC
            LIN = LIN + 4
            eNUM_IMOVEL = NUM_IMOVEL
            eQUADRA     = QUADRA
            ePARTE      = PARTE      
            SELE 6
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            IF FOUND()
               eQUANT_PARC = PARCELAS
            ELSE
               eQUANT_PARC = 0
            ENDIF
            SELE 8
            @ LIN,02  SAY DTOC(DATE())
            eVAZIO  = "00"
            eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
            @ LIN,20  SAY eINDENT
            @ LIN,35  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eQUANT_PARC,5))
            LIN = LIN + 1
            @ LIN,25  SAY "132"
            @ LIN,37  SAY ALLTRIM(sisMOEDA)
            @ LIN,72  SAY ALLTRIM(sisMOEDA)
            @ LIN,90  SAY VLR_PARC PICT "@R 999,999,999.99"
            IF eFAZ
               LIN = LIN + 3
               SELE 9
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
               LIN = LIN + 2
               SELE 8
            ELSE
               LIN = LIN + 9
            ENDIF
            SELE 6
            USE CAD_VEND INDEX CAD_VEND,DATA_VEN
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            ePROPR = NUM_CLIE
            SELE 5
            GOTO TOP
            SEEK ePROPR
            @ LIN,05  SAY CLIENTE
            LIN = LIN + 1
            IF OPCAO = "R"
               @ LIN,05  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
               LIN = LIN + 1
               @ LIN,05  SAY CEP_R PICT "@R 99999-999"
               @ LIN,15  SAY "- "+ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            ELSE
               @ LIN,05  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
               LIN = LIN + 1
               @ LIN,05  SAY CEP_C PICT "@R 99999-999"
               @ LIN,15  SAY "- "+ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            ENDIF
            LIN = LIN + 7
            @ LIN,00 SAY " "
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
      PPERG ( "Emite Mensagens nos Boletos (S/N) ? ")
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
         SET FILTER TO CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO = 0)
         GOTO TOP
         DO WHILE .NOT. EOF()
            LIN = PROW()
            @ LIN,02  SAY sisLETRAC+" "
            LIN = LIN + 1
            @ LIN,02  SAY "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO."
            @ LIN,02  SAY "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO." 
            @ LIN,90  SAY DATA_VENC
            LIN = LIN + 4
            eNUM_IMOVEL = NUM_IMOVEL
            eQUADRA     = QUADRA
            ePARTE      = PARTE      
            SELE 6
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            IF FOUND()
               eQUANT_PARC = PARCELAS
            ELSE
               eQUANT_PARC = 0
            ENDIF
            SELE 8
            @ LIN,02  SAY DTOC(DATE())
            eVAZIO  = "00"
            eINDENT = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eCLAS_PRO,5)))))+ALLTRIM(STR(eCLAS_PRO,5))+SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(QUADRA))))+ALLTRIM(QUADRA)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_IMOVEL))))+ALLTRIM(NUM_IMOVEL)+ALLTRIM(STR(PARTE,1))
            @ LIN,20  SAY eINDENT
            @ LIN,35  SAY ALLTRIM(NUM_PARC)+"/"+ALLTRIM(STR(eQUANT_PARC,5))
            LIN = LIN + 1
            @ LIN,25  SAY "132"
            @ LIN,37  SAY ALLTRIM(sisMOEDA)
            @ LIN,72  SAY ALLTRIM(sisMOEDA)
            @ LIN,90  SAY VLR_PARC PICT "@R 999,999,999.99"
            IF eFAZ
               LIN = LIN + 3
               SELE 9
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
               LIN = LIN + 2
               SELE 8
            ELSE
               LIN = LIN + 9
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
            @ LIN,05  SAY CLIENTE
            LIN = LIN + 1
            IF OPCAO = "R"
               @ LIN,05  SAY ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)
               LIN = LIN + 1
               @ LIN,05  SAY CEP_R PICT "@R 99999-999"
               @ LIN,15  SAY "- "+ALLTRIM(CIDADE_R)+" - "+ESTADO_R
            ELSE
               @ LIN,05  SAY ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)
               LIN = LIN + 1
               @ LIN,05  SAY CEP_C PICT "@R 99999-999"
               @ LIN,15  SAY "- "+ALLTRIM(CIDADE_C)+" - "+ESTADO_C
            ENDIF
            LIN = LIN + 7
            @ LIN,00 SAY " "
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
