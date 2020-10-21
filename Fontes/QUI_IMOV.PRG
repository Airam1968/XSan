*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : QUI_IMOV.PRG
* Data        : 01/01/99
* Objetivo    : QUITACAO DE IMOVEL                     
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION QUI_IMOV()
SAVE SCREEN TO TELA03
PTIT( "QUITACAO DA VENDA")
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
DO WHILE .T.
   eQUADRA = SPACE(1)
   eBLOCO  = SPACE(1)
   DO CASE
      CASE eTIPO_PRO = 1
         * Consulta de Loteamentos
         SELE 3
         SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
         GOTO TOP
         eQUADRA   = SPACE(5)
         PLQUA("01")
         PPERG ( "Deseja Selecionar Grupo (S/N) ? ")
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
            PPERG ( "Deseja Selecionar o "+sisUNID1+" (S/N) ? ")
            IF sisRESP = "S"
               @ 07,02 SAY sisUNID1+" : "
               SELE 1
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
               GOTO TOP
               IF EOF()
                  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
		  PMENSAG(" N∆o H†  "+sisUNID1+"  Dispon°veis  neste  Produto. ")
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
                        IF .NOT. QUITADO_1
                           IF ePROPR_1 <> 1   
                              ePARTE = 0
                              F_QUITA(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,0)
                           ENDIF
                        ELSE
                           PLQUA("01")
                           PMSGR( "Atená∆o !",3)
                           PMENSAG ( sisUNID1+" j† Quitado !")
                           PPERG ( "Deseja Alterar as Informaá‰es do Documento (S/N) ? ") 
                           IF sisRESP = "S"
                              IF F_BLOQR()
                                 @ 09,10 SAY "Copia Escritura (T/F) : " GET ESCRITU_01
                                 @ 11,10 SAY "Data da Escritura     : " GET ESCRITUR_1
                                 @ 13,10 SAY "Cartorio              : " GET CARTORIO_1
                                 @ 15,10 SAY "Livro                 : " GET LIVRO_1
                                 @ 17,10 SAY "Folha                 : " GET FOLHA_1
                                 READ
                                 UNLOCK
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ELSE
                     eOPCAO = 0
                     PLQUA("01")
                     @ 23,00 SAY "Informe a Quitaá∆o para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
                     READ
                     IF eOPCAO = 1
                        IF ePROPR_1 = 1
                           PLQUA("01")
                           PMSGR( "Atená∆o !",3)
                           PMENSAG ( sisUNID1+" N∆o Vendido !")
                           PPARA()
                        ELSE
                           IF .NOT. QUITADO_1
                              IF ePROPR_1 <> 1   
                                 ePARTE = 1
                                 F_QUITA(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,1)
                              ENDIF
                           ELSE
                              PLQUA("01")
                              PMSGR( "Atená∆o !",3)
                              PMENSAG ( sisUNID1+" j† Quitado !")
                              PPERG ( "Deseja Alterar as Informaá‰es do Documento (S/N) ? ") 
                              IF sisRESP = "S"
                                 IF F_BLOQR()
                                    @ 09,10 SAY "Copia Escritura (T/F) : " GET ESCRITU_01
                                    @ 11,10 SAY "Data da Escritura     : " GET ESCRITUR_1
                                    @ 13,10 SAY "Cartorio              : " GET CARTORIO_1
                                    @ 15,10 SAY "Livro                 : " GET LIVRO_1
                                    @ 17,10 SAY "Folha                 : " GET FOLHA_1
                                    READ
                                    UNLOCK
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ELSE
                        IF ePROPR_2 = 1
                           PLQUA("01")
                           PMSGR( "Atená∆o !",3)
                           PMENSAG ( sisUNID1+" N∆o Vendido !")
                           PPARA()
                        ELSE
                           IF .NOT. QUITADO_2
                              IF ePROPR_2 <> 1   
                                 ePARTE = 2
                                 F_QUITA(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,2)
                              ENDIF
                           ELSE
                              PLQUA("01")
                              PMSGR( "Atená∆o !",3)
                              PMENSAG ( sisUNID1+" j† Quitado !")
                              PPERG ( "Deseja Alterar as Informaá‰es da Escritura (S/N) ? ") 
                              IF sisRESP = "S"
                                 IF F_BLOQR()
                                    @ 09,10 SAY "Copia Escritura (T/F) : " GET ESCRITU_02
                                    @ 11,10 SAY "Data da Escritura     : " GET ESCRITUR_2
                                    @ 13,10 SAY "Cartorio              : " GET CARTORIO_2
                                    @ 15,10 SAY "Livro                 : " GET LIVRO_2
                                    @ 17,10 SAY "Folha                 : " GET FOLHA_2
                                    READ
                                    UNLOCK
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            F_QUIGER(eCLAS_PRO)
         ENDIF
      CASE eTIPO_PRO = 2
         * Consulta de Apartamentos
         SELE 4
         SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
         GOTO TOP
         eBLOCO    = SPACE(5)
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
            eBLOCO      = NUM_BLOCO
            eQUANTIDADE = QUANTIDADE
            PLQUA("01")
            PPERG ( "Deseja Selecionar "+sisUNID2+" (S/N) ? ")
            IF sisRESP = "S"
               @ 07,02 SAY sisUNID2+" : "
               SELE 1
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
               GOTO TOP
               IF EOF()
                  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
		  PMENSAG(" N∆o  H†  "+sisUNID2+"  Dispon°veis  neste  Produto. ")
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
                     PMENSAG ( sisUNID1+" N∆o Vendido !")
                     PPARA()
                  ELSE
                     IF .NOT. QUITADO_1
                        IF ePROPR_1 <> 1   
                           ePARTE = 0
                           F_QUITA(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
                        ENDIF
                     ELSE
                        PLQUA("01")
                        PMSGR( "Atená∆o !",3)
                        PMENSAG ( sisUNID1+" j† Quitado !")
                        PPERG ( "Deseja Alterar as Informaá‰es da Escritura (S/N) ? ") 
                        IF sisRESP = "S"
                           IF F_BLOQR()
                              @ 09,10 SAY "Copia Escritura (T/F) : " GET ESCRITU_01
                              @ 11,10 SAY "Data da Escritura     : " GET ESCRITUR_1
                              @ 13,10 SAY "Cartorio              : " GET CARTORIO_1
                              @ 15,10 SAY "Livro                 : " GET LIVRO_1
                              @ 17,10 SAY "Folha                 : " GET FOLHA_1
                              READ
                              UNLOCK
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            F_QUIGER(eCLAS_PRO)
         ENDIF
      CASE eTIPO_PRO = 3
         * Consulta de Salas Comerciais
         SELE 4
         SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
         GOTO TOP
         eBLOCO    = SPACE(5)
         PLQUA("01")
         PPERG ( "Deseja Selecionar Bloco (S/N) ? ")
         IF sisRESP = "S"
            @ 06,02 SAY "Bloco   : "
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
            PLQUA("01")
            PPERG ( "Deseja Selecionar Apto. (S/N) ? ")
            IF sisRESP = "S"
               @ 07,02 SAY "Apto.   : "
               SELE 1
               SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
               GOTO TOP
               IF EOF()
                  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
		  PMENSAG("N∆o H† Im¢veis Dispon°veis neste Empreendimento.")
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
                  CAB[3]="Apto."
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
                     IF .NOT. QUITADO_1
                        IF ePROPR_1 <> 1   
                           ePARTE = 0
                           F_QUITA(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0)
                        ENDIF
                     ELSE
                        PLQUA("01")
                        PMSGR( "Atená∆o !",3)
                        PMENSAG("Imovel j† Quitado !")
                        PPERG ( "Deseja Alterar as Informaá‰es da Escritura (S/N) ? ") 
                        IF sisRESP = "S"
                           IF F_BLOQR()
                              @ 09,10 SAY "Copia Escritura (T/F) : " GET ESCRITU_01 
                              @ 11,10 SAY "Data da Escritura     : " GET ESCRITUR_1
                              @ 13,10 SAY "Cartorio              : " GET CARTORIO_1
                              @ 15,10 SAY "Livro                 : " GET LIVRO_1
                              @ 17,10 SAY "Folha                 : " GET FOLHA_1
                              READ
                              UNLOCK
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            F_QUIGER(eCLAS_PRO)
         ENDIF
      OTHERWISE
         * Consulta de Casa/Terrenos/etc...
         DO PDESENV
   ENDCASE
   PLQUA("01")
   PPERG ( "Deseja Continuar no mesmo Empreendimento (S/N) ? ")
   IF sisRESP = "S"
      @ 06,00 CLEAR TO 20,79
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_QUITA
PARAMETERS pT_PRO,pQUADRA,pUNIDADE,pM_UNID
eMSG = "QUITAÄ«O DA VENDA DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pM_UNID=0,"Proprietario Unico",IIF(pM_UNID=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
PCENTRA( eMSG,9)
PLQUA("01")
PPERG ( "Confirma o Imovel (S/N) ? ")
IF sisRESP = "N"
   RETURN 0
ENDIF
PLQUA("01")
SELE 6
GOTO TOP
SEEK STR(pT_PRO,5)+pQUADRA+pUNIDADE+STR(pM_UNID,1)
eNUM_CLIE = NUM_CLIE
eNUM_PARC = PARCELAS
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK eNUM_CLIE
@ 11,02 SAY "Nome :" GET CLIENTE
CLEAR GETS
PLQUA("01")
PPERG ( "Confirma a Quitaá∆o (S/N) ? ")
IF sisRESP = "N"
   RETURN 0
ENDIF
IF eNUM_PARC <> 0
   CLEAR GETS
   PLQUA("01")
   PMENSAG("Aguarde a Confirmaá∆o das Parcelas Pagas ...")
   SELE 8
   eIND_CLAS = STR(pT_PRO,5)
   eIND_QUAD = pQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID .AND. VLR_PGTO = 0 .AND. ATIVA
   GOTO TOP
   IF .NOT. EOF()
      PLQUA("01")
	  PMSGR( "Atená∆o !",3)
	  PMENSAG("H† Parcelas em Debito, Quitaá∆o Cancelada !")
      PPARA()
	  RETURN 0
   ELSE
      PLQUA("01")
	  PMENSAG("Aguarde Efetuando Quitaá∆o !")
      SELE 8
      SET FILTER TO 
      GOTO TOP
      SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID 
      GOTO TOP
      IF EOF()
         PLQUA("01")
	 PMSGR( "Atená∆o !",3)
	 PMENSAG("Nao H† Parcelas !")
         PPARA()
	 RETURN 0
      ELSE 
         IF F_BLOQA()
            REPLACE ALL ATIVA WITH .F.
            UNLOCK
         ENDIF
      ENDIF
      eVLR_PARC = VLR_PARC
      SELE 6
      IF F_BLOQR()
         REPLACE QUITADO     WITH .T.        
         UNLOCK
      ENDIF
      SELE 1
      IF pM_UNID = 0 .OR. pM_UNID = 1
         IF F_BLOQR()
            REPLACE QUITADO_1 WITH .T.
            UNLOCK
         ENDIF
      ELSE
         IF F_BLOQR()
            REPLACE QUITADO_2 WITH .T.
            UNLOCK
         ENDIF
      ENDIF
      SELE 7
      APPEND BLANK
      IF F_BLOQR()
         REPLACE CLASSE      WITH STR(pT_PRO,5),;
                 QUADRA      WITH pQUADRA,;
                 NUM_IMOVEL  WITH pUNIDADE
         REPLACE PARTE       WITH pM_UNID,;
                 NUM_CLIE    WITH eNUM_CLIE,;
                 DATA_RENEG  WITH DATE()
         REPLACE VLR_PARCEL  WITH eVLR_PARC,;
                 TIPO_RENEG  WITH "Q",;
                 NUM_RENEG   WITH 1
         UNLOCK
      ENDIF
      PLQUA("01")
      PMSGR( "Quitaá∆o Concluida !",3)
      F_LACESSO(sisUSUARIO,"ROTINAS ESPECIAIS","QUITACAO IMOVEL : "+ALLTRIM(STR(pT_PRO,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pM_UNID,1)))
   ENDIF
ELSE
   CLEAR GETS
   PLQUA("01")
   PMENSAG("Efetuado Quitaá∆o do IM‡VEL ...")
   SELE 6
   IF F_BLOQR()
      REPLACE QUITADO     WITH .T.        
      UNLOCK
   ENDIF
   SELE 1
   IF pM_UNID = 0 .OR. pM_UNID = 1
      IF F_BLOQR()
         REPLACE QUITADO_1 WITH .T.
         UNLOCK
      ENDIF
   ELSE
      IF F_BLOQR()
         REPLACE QUITADO_2 WITH .T.
         UNLOCK
      ENDIF
   ENDIF
   SELE 7
   APPEND BLANK
   IF F_BLOQR()
      REPLACE CLASSE      WITH STR(pT_PRO,5),;
              QUADRA      WITH pQUADRA,;
              NUM_IMOVEL  WITH pUNIDADE
      REPLACE PARTE       WITH pM_UNID,;
              NUM_CLIE    WITH eNUM_CLIE,;
              DATA_RENEG  WITH DATE()
      REPLACE TIPO_RENEG  WITH "Q",;
              NUM_RENEG   WITH 1
      UNLOCK
   ENDIF
   PLQUA("01")
   PMSGR( "Quitaá∆o Concluida !",3)
ENDIF
RETURN 0
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_QUIGER
PARAMETERS pT_PRO
PLQUA("01")
PPERG ( "Confirma o Quitaá∆o Autom†tica de Todo o Empreendimento (S/N) ? ")
IF sisRESP = "N"
   RETURN 0
ENDIF
SELE 6
GOTO TOP
SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. .NOT. QUITADO
GOTO TOP
DO WHILE .NOT. EOF()
   pQUADRA  = QUADRA
   pUNIDADE = NUM_IMOVEL
   pM_UNID  = PARTE
   eMSG = "QUITAÄ«O DA VENDA DA UNIDADE No. "+ALLTRIM(pUNIDADE)+" - "+IIF(pM_UNID=0,"Proprietario Unico",IIF(pM_UNID=1,"1o. PROPRIETARIO","2o. PROPRIETARIO"))
   PCENTRA( eMSG,9)
   eNUM_CLIE = NUM_CLIE
   eNUM_PARC = PARCELAS
   SELE 5
   USE CONTATO INDEX NUM_CLIE,CONTATO
   GOTO TOP
   SEEK eNUM_CLIE
   @ 11,02 SAY "Nome :" GET CLIENTE
   CLEAR GETS
   IF eNUM_PARC <> 0
      CLEAR GETS
      PLQUA("01")
	  PMENSAG("Aguarde a Confirmaá∆o das Parcelas Pagas ...")
      SELE 8
      eIND_CLAS = STR(pT_PRO,5)
      eIND_QUAD = pQUADRA
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      USE &eARQ INDEX &eIND1,&eIND2,&eIND3
      SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID .AND. VLR_PGTO = 0 .AND. DATA_PGTO = CTOD("") .AND. ATIVA
      GOTO TOP
      IF .NOT. EOF()
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
         PMENSAG("H† Parcelas em Debito, Quitaá∆o Cancelada !")
      ELSE
         PLQUA("01")
         PMENSAG("Aguarde Efetuando Quitaá∆o !")
         SELE 8
         SET FILTER TO 
         GOTO TOP
         SET FILTER TO CLASSE = STR(pT_PRO,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pM_UNID 
         GOTO TOP
         IF EOF()
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("Nao H† Parcelas !")
         ELSE 
            IF F_BLOQA()
               REPLACE ALL ATIVA WITH .F.
               UNLOCK
            ENDIF
         ENDIF
         eVLR_PARC = VLR_PARC
         SELE 6
         IF F_BLOQR()
            REPLACE QUITADO     WITH .T.        
            UNLOCK
         ENDIF
         SELE 1
         IF pM_UNID = 0 .OR. pM_UNID = 1
            IF F_BLOQR()
               REPLACE QUITADO_1 WITH .T.
               UNLOCK
            ENDIF
         ELSE
            IF F_BLOQR()
               REPLACE QUITADO_2 WITH .T.
               UNLOCK
            ENDIF
         ENDIF
         SELE 7
         APPEND BLANK
         IF F_BLOQR()
            REPLACE CLASSE      WITH STR(pT_PRO,5),;
                    QUADRA      WITH pQUADRA,;
                    NUM_IMOVEL  WITH pUNIDADE
            REPLACE PARTE       WITH pM_UNID,;
                    NUM_CLIE    WITH eNUM_CLIE,;
                    DATA_RENEG  WITH DATE()
            REPLACE VLR_PARCEL  WITH eVLR_PARC,;
                    TIPO_RENEG  WITH "Q",;
                    NUM_RENEG   WITH 1
            UNLOCK
         ENDIF
         F_LACESSO(sisUSUARIO,"ROTINAS ESPECIAIS","QUITACAO IMOVEL : "+ALLTRIM(STR(pT_PRO,5))+"/"+ALLTRIM(pQUADRA)+"/"+ALLTRIM(pUNIDADE)+"/"+ALLTRIM(STR(pM_UNID,1)))
      ENDIF
   ELSE
   ENDIF
   SELE 6
   SKIP
ENDDO
PLQUA("01")
PMSGR( "Quitaá∆o Concluida !",3)
RETURN 0
*************************  fim  *********************************
