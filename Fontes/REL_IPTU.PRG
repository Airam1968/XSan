*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_IPTU.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CADASTRO PARA PREFEITURA - I.P.T.U.
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_IPTU()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE CADASTRO PARA PREFEITURA - I.P.T.U.")
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
USE TIPO_PRO INDEX TIPO_PRO
GOTO TOP
SELE 7
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
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
   eDESC_PRO  = DESCRICAO
   eCOD_CONTA = COD_CONTA
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
      eOPCAO = "A"
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
            @ 07,02 SAY sisUNID1+"   : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
			   PMSGR( "Atená∆o !",3)
			   PMENSAG("N∆o H† Imoveis Dispon°veis !")
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
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL 
            GOTO TOP
         ELSE
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA 
            GOTO TOP
         ENDIF
      ELSE
         eOPCAO = SPACE(1)
         PLQUA("01") 
         @ 23,00 SAY "Relacao de (V)endidos,(D)isponiveis ou (A)mbos (V/D/A) ? " GET eOPCAO PICT "@!X" VALID eOPCAO $("VDA")
         READ
         SELE 1
         DO CASE
            CASE eOPCAO = "A"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5)  
            CASE eOPCAO = "D"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. IIF(STATUS = 0,(PROPRIET_1 =  1),(PROPRIET_1 =  1 .OR. PROPRIET_2 =  1))
            CASE eOPCAO = "V"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1)) 
         ENDCASE
         GOTO TOP
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
      eOPCAO = "A"
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO2+" (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO2+"  : "
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
            @ 07,02 SAY sisUNID2+"  : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
			   PMSGR( "Atená∆o !",3)
			   PMENSAG("N∆o H† Imoveis Dispon°veis !")
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
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL 
            GOTO TOP
         ELSE
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA 
            GOTO TOP
         ENDIF
      ELSE
         eOPCAO = SPACE(1)
         PLQUA("01") 
         @ 23,00 SAY "Relacao de (V)endidos,(D)isponiveis ou (A)mbos (V/D/A) ? " GET eOPCAO PICT "@!X" VALID eOPCAO $("VDA")
         READ
         SELE 1
         DO CASE
            CASE eOPCAO = "A"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5)  
            CASE eOPCAO = "D"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. IIF(STATUS = 0,(PROPRIET_1 =  1),(PROPRIET_1 =  1 .OR. PROPRIET_2 =  1))
            CASE eOPCAO = "V"
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1)) 
         ENDCASE
         GOTO TOP
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Vendas de : " GET eDATA_INI
   @ 08,27 SAY "Ate : "       GET eDATA_FIM
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
eIMP_VENDA = .F.
PLQUA("01")
PPERG ( "Deseja a Impress∆o da Data da Venda (S/N) ? ")
IF sisRESP = "S"
   eIMP_VENDA = .T.
ENDIF
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"IPTU.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "IPTU.TXT"+SPACE(10) 
ENDIF 
eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja extrato em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO
SELE 1
GOTO TOP
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† Relat¢rio Neste Per°odo !")
      PPARA()
   ELSE
      IF eSAIDA = "ARQUIVO"
         SET PRINT ON
         SET DEVICE TO PRINT
         SET PRINTER TO &eARQUIVO
      ELSE
         PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
         PPARA()
         DO PIMPR
         SET DEVICE TO PRINT
      ENDIF
   ENDIF   
NPAG = 0
eFAZ = .T.
DO WHILE .NOT. EOF()
   SELE 6
   SEEK eTIPO_PRO
   eTIPO_PRD = DESCRICAO
   SELE 1
   NPAG = NPAG + 1
   @ 00,00  SAY sisLETRAN+" "
   @ 01,00  SAY sisLETRAE+sisTIT1+sisLETRAC
   @ 02,40  SAY sisLETRAE+"Relatorio de Cadastro para I.P.T.U."+sisLETRAC
   @ 03,40  SAY sisLETRAE+ALLTRIM(STR(eCLAS_PRO,5))+" - "+ALLTRIM(eDESC_PRO)+sisLETRAC
   @ 04,00  SAY sisLETRAE+"Emitido em  : "+DTOC(DATE())+sisLETRAC
   @ 04,55  SAY sisLETRAE+"Pagina : "+STR(NPAG,5)+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   LIN = 6
   DO WHILE .NOT. EOF() .AND. LIN <= 50
      SELE 1
      eSTATUS     = STATUS
      eQUADRA     = QUADRA
      eNUM_IMOVEL = NUM_IMOVEL
      IF eSTATUS = 0
         ePARTE = 0
      ELSE
         ePARTE = 1                                                    
      ENDIF
      DO CASE
         CASE eOPCAO = "A"
              SELE 7
              SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
              eDATA_VENDA = DATA_VENDA
              IF eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS = 0
                 SELE 1
                 @ LIN,02  SAY "INSCRICAO IMOB. : "
                 @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                 @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                 IF eTIPO_PRO = 1
                    @ LIN,40  SAY sisGRUPO1+" : "+QUADRA
                    @ LIN,60  SAY sisUNID1+" : "+NUM_IMOVEL
                 ELSE
                    @ LIN,40  SAY sisGRUPO2+" : "+QUADRA
                    @ LIN,60  SAY sisUNID2+" : "+NUM_IMOVEL
                 ENDIF
                 IF eIMP_VENDA
                    @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                 ENDIF
                 SELE 1
                 LIN = LIN + 1
                 ePROPRIET_1 = PROPRIET_1
                 SELE 5
                 SEEK ePROPRIET_1
                 @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                 @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                 @ LIN,100  SAY "INSCR/RG: "+RG
                 LIN = LIN + 1
                 IF OPCAO = "R"
                    @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                    +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                 ELSE
                    @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                    +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                 ENDIF
                 LIN = LIN + 1
                 SELE 1
                 IF ESCRITUR_1 <> CTOD("")
                     @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_1)+" - CARTORIO : ";
                     +CARTORIO_1+" - Folhas : "+FOLHA_1+" - Livro : "+LIVRO_1
                 ELSE
                    @ LIN,02  SAY "COMPROMISSARIO"
                 ENDIF
                 LIN = LIN + 1
                 @ LIN,00  SAY REPLICATE("-",135)
                 LIN = LIN + 1
              ELSE
                 IF eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS <> 0
                    * PRIMEIRA PARTE
                    SELE 7
                    SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+"1"
                    eDATA_VENDA = DATA_VENDA
                    SELE 1
                    @ LIN,02  SAY "INSCRICAO IMOB. : "
                    @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                    @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                    IF eTIPO_PRO = 1
                       @ LIN,40  SAY sisGRUPO1+" : "+QUADRA
                       @ LIN,60  SAY sisUNID1+" : "+NUM_IMOVEL
                    ELSE
                       @ LIN,40  SAY sisGRUPO2+" : "+QUADRA
                       @ LIN,60  SAY sisUNID2+" : "+NUM_IMOVEL
                    ENDIF
                    ePARTE = 1
                    IF eIMP_VENDA 
                       @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    ePROPRIET_1 = PROPRIET_1
                    SELE 5
                    SEEK ePROPRIET_1
                    @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                    @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                    @ LIN,100  SAY "INSCR/RG: "+RG
                    LIN = LIN + 1
                    IF OPCAO = "R"
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                       +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                    ELSE
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                      +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    IF ESCRITUR_1 <> CTOD("")
                       @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_1)+" - CARTORIO : ";
                       +CARTORIO_1+" - Folhas : "+FOLHA_1+" - Livro : "+LIVRO_1
                    ELSE
                       @ LIN,02  SAY "COMPROMISSARIO"
                    ENDIF
                    LIN = LIN + 1
                    @ LIN,00  SAY REPLICATE("-",135)
                    LIN = LIN + 1
                    * SEGUNDA PARTE
                    SELE 7
                    SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+"2"
                    eDATA_VENDA = DATA_VENDA
                    SELE 1
                    @ LIN,02  SAY "INSCRICAO IMOB. : "
                    @ LIN,20  SAY INSCRICA_2 PICT "@R 99.999.999.99.9"
                    @ LIN,20  SAY INSCRICA_2 PICT "@R 99.999.999.99.9"
                    @ LIN,40  SAY sisGRUPO2+" : "+QUADRA
                    @ LIN,60  SAY sisUNID2+" : "+NUM_IMOVEL
                    ePARTE = 1
                    IF eIMP_VENDA 
                       @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    ePROPRIET_2 = PROPRIET_2
                    SELE 5
                    SEEK ePROPRIET_2
                    @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                    @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                    @ LIN,100  SAY "INSCR/RG: "+RG
                    LIN = LIN + 1
                    IF OPCAO = "R"
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                       +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                    ELSE
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                      +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    IF ESCRITUR_2 <> CTOD("")
                       @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_2)+" - CARTORIO : ";
                       +CARTORIO_2+" - Folhas : "+FOLHA_2+" - Livro : "+LIVRO_2
                    ELSE
                       @ LIN,02  SAY "COMPROMISSARIO"
                    ENDIF
                    LIN = LIN + 1
                    @ LIN,00  SAY REPLICATE("-",135)
                    LIN = LIN + 1
                 ENDIF
              ENDIF
         CASE eOPCAO = "D"
              SELE 7
              SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
              IF .NOT. FOUND()
                 SELE 1
                 @ LIN,02  SAY "INSCRICAO IMOB. : "
                 @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                 @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                 IF eTIPO_PRO = 1
                    @ LIN,40  SAY sisGRUPO1+" : "+QUADRA
                    @ LIN,60  SAY sisUNID1+" : "+NUM_IMOVEL
                 ELSE
                    @ LIN,40  SAY sisGRUPO2+" : "+QUADRA
                    @ LIN,60  SAY sisUNID2+" : "+NUM_IMOVEL
                 ENDIF
                 IF eIMP_VENDA
                    @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                 ENDIF
                 SELE 1
                 LIN = LIN + 1
                 ePROPRIET_1 = PROPRIET_1
                 SELE 5
                 SEEK ePROPRIET_1
                 @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                 @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                 @ LIN,100  SAY "INSCR/RG: "+RG
                 LIN = LIN + 1
                 IF OPCAO = "R"
                    @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                    +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                 ELSE
                    @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                    +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                 ENDIF
                 LIN = LIN + 1
                 SELE 1
                 IF ESCRITUR_1 <> CTOD("")
                     @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_1)+" - CARTORIO : ";
                     +CARTORIO_1+" - Folhas : "+FOLHA_1+" - Livro : "+LIVRO_1
                 ELSE
                    @ LIN,02  SAY "COMPROMISSARIO"
                 ENDIF
                 LIN = LIN + 1
                 @ LIN,00  SAY REPLICATE("-",135)
                 LIN = LIN + 1
              ELSE
                 * PRIMEIRA PARTE
                 SELE 7
                 SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+"1"
                 IF .NOT. FOUND()
                    SELE 1
                    @ LIN,02  SAY "INSCRICAO IMOB. : "
                    @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                    @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                    @ LIN,40  SAY sisGRUPO1+" : "+QUADRA
                    @ LIN,60  SAY sisUNID1+" : "+NUM_IMOVEL
                    ePARTE = 1
                    IF eIMP_VENDA 
                       @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    ePROPRIET_1 = PROPRIET_1
                    SELE 5
                    SEEK ePROPRIET_1
                    @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                    @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                    @ LIN,100  SAY "INSCR/RG: "+RG
                    LIN = LIN + 1
                    IF OPCAO = "R"
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                       +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                    ELSE
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                      +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    IF ESCRITUR_1 <> CTOD("")
                       @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_1)+" - CARTORIO : ";
                       +CARTORIO_1+" - Folhas : "+FOLHA_1+" - Livro : "+LIVRO_1
                    ELSE
                       @ LIN,02  SAY "COMPROMISSARIO"
                    ENDIF
                    LIN = LIN + 1
                    @ LIN,00  SAY REPLICATE("-",135)
                    LIN = LIN + 1
                 ENDIF
                 * SEGUNDA PARTE
                 SELE 7
                 SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+"2"
                 IF .NOT. FOUND()
                    SELE 1
                    @ LIN,02  SAY "INSCRICAO IMOB. : "
                    @ LIN,20  SAY INSCRICA_2 PICT "@R 99.999.999.99.9"
                    @ LIN,20  SAY INSCRICA_2 PICT "@R 99.999.999.99.9"
                    @ LIN,40  SAY "QUADRA : "+QUADRA
                    @ LIN,60  SAY "LOTE : "+NUM_IMOVEL
                    LIN = LIN + 1
                    ePARTE = 1
                    SELE 1
                    ePROPRIET_2 = PROPRIET_2
                    SELE 5
                    SEEK ePROPRIET_2
                    @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                    @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                    @ LIN,100  SAY "INSCR/RG: "+RG
                    LIN = LIN + 1
                    IF OPCAO = "R"
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                       +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                    ELSE
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                      +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    IF ESCRITUR_2 <> CTOD("")
                       @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_2)+" - CARTORIO : ";
                       +CARTORIO_2+" - Folhas : "+FOLHA_2+" - Livro : "+LIVRO_2
                    ELSE
                       @ LIN,02  SAY "COMPROMISSARIO"
                    ENDIF
                    LIN = LIN + 1
                    @ LIN,00  SAY REPLICATE("-",135)
                    LIN = LIN + 1
                 ENDIF
              ENDIF
         CASE eOPCAO = "V"
              SELE 7
              SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
              eDATA_VENDA = DATA_VENDA
              IF FOUND() .AND. eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS = 0
                 SELE 1
                 @ LIN,02  SAY "INSCRICAO IMOB. : "
                 @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                 @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                 IF eTIPO_PRO = 1
                    @ LIN,40  SAY sisGRUPO1+" : "+QUADRA
                    @ LIN,60  SAY sisUNID1+" : "+NUM_IMOVEL
                 ELSE
                    @ LIN,40  SAY sisGRUPO2+" : "+QUADRA
                    @ LIN,60  SAY sisUNID2+": "+NUM_IMOVEL
                 ENDIF
                 IF eIMP_VENDA
                    @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                 ENDIF
                 SELE 1
                 LIN = LIN + 1
                 ePROPRIET_1 = PROPRIET_1
                 SELE 5
                 SEEK ePROPRIET_1
                 @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                 @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                 @ LIN,100  SAY "INSCR/RG: "+RG
                 LIN = LIN + 1
                 IF OPCAO = "R"
                    @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                    +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                 ELSE
                    @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                    +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                 ENDIF
                 LIN = LIN + 1
                 SELE 1
                 IF ESCRITUR_1 <> CTOD("")
                     @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_1)+" - CARTORIO : ";
                     +CARTORIO_1+" - Folhas : "+FOLHA_1+" - Livro : "+LIVRO_1
                 ELSE
                    @ LIN,02  SAY "COMPROMISSARIO"
                 ENDIF
                 LIN = LIN + 1
                 @ LIN,00  SAY REPLICATE("-",135)
                 LIN = LIN + 1
              ELSE
                 * PRIMEIRA PARTE
                 SELE 7
                 SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+"1"
                 eDATA_VENDA = DATA_VENDA
                 IF FOUND() .AND. eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS <> 0
                    SELE 1
                    @ LIN,02  SAY "INSCRICAO IMOB. : "
                    @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                    @ LIN,20  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
                    @ LIN,40  SAY sisGRUPO1+" : "+QUADRA
                    @ LIN,60  SAY sisUNID1+" : "+NUM_IMOVEL
                    ePARTE = 1
                    IF eIMP_VENDA 
                       @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    ePROPRIET_1 = PROPRIET_1
                    SELE 5
                    SEEK ePROPRIET_1
                    @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                    @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                    @ LIN,100  SAY "INSCR/RG: "+RG
                    LIN = LIN + 1
                    IF OPCAO = "R"
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                       +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                    ELSE
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                      +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    IF ESCRITUR_1 <> CTOD("")
                       @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_1)+" - CARTORIO : ";
                       +CARTORIO_1+" - Folhas : "+FOLHA_1+" - Livro : "+LIVRO_1
                    ELSE
                       @ LIN,02  SAY "COMPROMISSARIO"
                    ENDIF
                    LIN = LIN + 1
                    @ LIN,00  SAY REPLICATE("-",135)
                    LIN = LIN + 1
                 ENDIF
                 * SEGUNDA PARTE
                 SELE 7
                 SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+"2"
                 eDATA_VENDA = DATA_VENDA
                 IF FOUND() .AND. eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS <> 0
                    SELE 1
                    @ LIN,02  SAY "INSCRICAO IMOB. : "
                    @ LIN,20  SAY INSCRICA_2 PICT "@R 99.999.999.99.9"
                    @ LIN,20  SAY INSCRICA_2 PICT "@R 99.999.999.99.9"
                    @ LIN,40  SAY sisGRUPO2+" : "+QUADRA
                    @ LIN,60  SAY sisUNID2+" : "+NUM_IMOVEL
                    ePARTE = 1
                    IF eIMP_VENDA 
                       @ LIN,85 SAY "DATA VENDA : "+DTOC(eDATA_VENDA)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    ePROPRIET_2 = PROPRIET_2
                    SELE 5
                    SEEK ePROPRIET_2
                    @ LIN,02  SAY "COMPRADOR : "+ALLTRIM(CLIENTE)
                    @ LIN,70  SAY "CGC/CPF : "+CPF_CGC
                    @ LIN,100  SAY "INSCR/RG: "+RG
                    LIN = LIN + 1
                    IF OPCAO = "R"
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_R)+" - "+ALLTRIM(BAIRRO_R)+" - ";
                       +ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)+" - "+SUBSTR(CEP_R,1,5)+"-"+SUBSTR(CEP_R,6,3)
                    ELSE
                       @ LIN,02  SAY "ENDERECO  : "+ALLTRIM(END_C)+" - "+ALLTRIM(BAIRRO_C)+" - ";
                      +ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)+" - "+SUBSTR(CEP_C,1,5)+"-"+SUBSTR(CEP_C,6,3)
                    ENDIF
                    LIN = LIN + 1
                    SELE 1
                    IF ESCRITUR_2 <> CTOD("")
                       @ LIN,02  SAY "ESCRITURA EM : "+DTOC(ESCRITUR_2)+" - CARTORIO : ";
                       +CARTORIO_2+" - Folhas : "+FOLHA_2+" - Livro : "+LIVRO_2
                    ELSE
                       @ LIN,02  SAY "COMPROMISSARIO"
                    ENDIF
                    LIN = LIN + 1
                    @ LIN,00  SAY REPLICATE("-",135)
                    LIN = LIN + 1
                 ENDIF
              ENDIF
      ENDCASE
      SELE 1
      SKIP
    DO PTECLA
    IF sisINTERR
       IF eSAIDA = "ARQUIVO"
	  SET PRINT OFF
	  SET DEVICE TO SCREEN
       ELSE
	  SET DEVICE TO SCREEN
       ENDIF
       CLOSE ALL
       RETURN nil
    ENDIF
   ENDDO
   @ 57,00 SAY REPLICATE("=",135)
   @ 58,104 SAY sisTIT3+sisLETRAN+" "
   @ 00,00 SAY " "
ENDDO
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
