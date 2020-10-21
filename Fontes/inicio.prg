//************************* inicio ************************************
//*-------------------- parametros do sistema  ---------------


FUNCTION MAIN()
SETMODE(25,80)
SET STATUS OFF
SET DATE BRITISH
SET CENTURY ON
SET BELL OFF
SET SAFETY OFF
SET DELETE ON
SET MESSAGE TO 21
SET WRAP ON
SET COLOR TO W/N,+W/R
SET EXCLUSIVE OFF
dbcloseall()
CLEAR ALL
CLEAR
//*--------------------- fim  --------------------------------------
//*--------------- variaveis gerais do sistema  --------------------

PUBLIC sisBAR,sisSINO,sisDATA,sisHORA,sisSENHA,sisTRUE
PUBLIC sisSIS,sisMOD,sisNIV,sisLETRAC,sisLETRAE,sisLETRAN
PUBLIC sisTIT1,sisTIT2,sisTIT3,sisDISK,sisINTERR,sisMOEDA
PUBLIC sisVERSAO,pEXT,sisDADOS,sisUSUARIO,sisCGC_CPF
PUBLIC pNUM_EXT1,pNUM_EXT2,pNUM_EXT3,sisCIDADE,sisESTADO
PUBLIC sisPROD1,sisPROD2,sisGRUPO1,sisGRUPO2,sisUNID1,sisUNID2
PUBLIC sisCONTATO,sisTEL,sisEMAIL,sisDRIVE_REL,sisPASTA_REL,sisBLOCO_CLIE
sisBAR=REPLICATE(CHR(177),77)              && barra da tela.
sisSINO=CHR(7)                             && apito.
sisOP=SPACE(1)                             && variavel de opcao.
sisRESP=SPACE(1)                           && resposta do tipo (S/N).
sisDATA=DTOC(DATE())                       && data do sistema.
sisEXT=SPACE(40)                           && data por extenso.
sisHORA=TIME()                             && hora de entrada no sistema.
sisDISK=SPACE(1)                           && drive ativo.
sisSENHA=SPACE(10)                         && senha do usuario.
sisUSUARIO=SPACE(10)                       && nome do usuario.
sisTRUE=.F.                                && testa validade da senha.
sisSIS=SPACE(1)                            && sistema permitido a semha fornecida.
sisMOD=SPACE(1)                            && modulo de acesso no sistema permitido.
sisNIV=SPACE(1)                            && nivel de acesso dentro do modulo do sistema.
sisTENT=1                                  && tentativas de senhas Inv lidas.
sisLETRAE=CHR(14)                          && letra expandida.
sisLETRAC=CHR(15)                          && letra comprimida.
sisLETRAN=CHR(18)                          && letra normal.
sisCONTATO=SPACE(30)                       && Pessoa de Contato
sisTEL=SPACE(14)                           && Telefone da Empresa
sisEMAIL=SPACE(30)                         && Email
sisDRIVE_REL=SPACE(1)                      && DRIVE REL
sisPASTA_REL=SPACE(30)                     && PASTA REL
SELE 1
USE STATUS
IF NETERR() 
   PMENSAG("ERRO NA ABERTURA DO ARQUIVO STATUS.DBF") 
   PPARA()
   dbcloseall()
   CLOSE DATABASES 
   QUIT 
ENDIF 
GOTO TOP
eDATA_S=DATA_S                             && Data Ultima atualiza‹¨«‹¨«o
sisTIT1=ALLTRIM(EMPRESA)                   && titulo-1.
sisTIT2=ALLTRIM(SISTEMA)                   && titulo-2.
sisTIT3=ALLTRIM(DEPTO)                     && titulo-3.
sisCGC_CPF=CGC_CPF                         && CGC/CPF da Empresa
sisVERSAO=ALLTRIM(VERSAO)                  && Versao atual
sisMOEDA=ALLTRIM(MOEDA)                    && moeda corrente
pEXT=SPACE(20)                             && Data por Extenso
sisCIDADE=ALLTRIM(CIDADE)                  && Cidade da Empresa
sisESTADO=ALLTRIM(ESTADO)                  && Estado da Empresa
sisDADOS="C:\SIS\SAN\EXE\"                 && Subdiretorio de Dados
sisCONTATO="CONTATO"                       && Contato da Empresa
sisTEL=TEL1                                && Telefone da Empresa
sisEMAIL=EMAIL                             && Email
sisBLOCO_CLIE=BLOCO_CLIE                   && Bloco de Notas do Cliente
sisDRIVE_REL=ALLTRIM(DRIVE_REL)            && Drive para Relat¢rios
sisPASTA_REL=ALLTRIM(PASTA_REL)            && Pasta para Relat¢rios
SELE 2
USE TIPO_PRO INDEX TIPO_PRO
GOTO TOP
SEEK 2
IF FOUND()
   sisPROD1 = PRODUTO                      && Descricao Produto Tipo 1.
   sisGRUPO1= GRUPO                        && Descricao Grupo Produto 1.
   sisUNID1 = UNIDADE                      && Descricao Unidade Grupo 1.
ELSE
   sisPROD1 = "PRODUTO"                    && Descricao Produto Tipo 1.
   sisGRUPO1= "QUADRA"                     && Descricao Grupo Produto 1.
   sisUNID1 = "LOTE"                       && Descricao Unidade Grupo 1.
ENDIF
GOTO TOP
SEEK 2
IF FOUND()
   sisPROD2 = PRODUTO                      && Descricao Produto Tipo 2.
   sisGRUPO2= GRUPO                        && Descricao Grupo Produto 2.
   sisUNID2 = UNIDADE                      && Descricao Unidade Grupo 2.
ELSE
   sisPROD2 = "PRODUTO"                    && Descricao Produto Tipo 2.
   sisGRUPO2= "BLOCO"                      && Descricao Grupo Produto 2.
   sisUNID2 = "UNID."                      && Descricao Unidade Grupo 2.
ENDIF

//*---------------------  fim  -----------------------------
//*-----------------  rotina de arquivo --------------------
CLEAR
PTELA (sisTIT1,sisTIT2,sisTIT3,sisBAR)
SET COLOR TO R+/N+*
PCENTRA ("Copyright 1995/2020 By, All Rights Reserved ",07)
PCENTRA ("SFin - FINANCIAMENTOS",09)
SET COLOR TO B+/N+
PCENTRA ("Gitano Software",12)
SET COLOR TO GR+/N+
PCENTRA ("Rua Palmares, 116 - 12 - SJCampos - SP",14)
PCENTRA ("Cep 12235-620",15)
PCENTRA ("Fone : (012) 3307-4310",17)
PCENTRA ("Whatsapp : (12) 99664-6156",18)
PCENTRA ("WebSite : https://www.gitanosoftware.com.br/",19)
PCENTRA ("E-MAIL\SKYPE\LINKEDIN : airam1968@terra.com.br",21)
eFAZ = .F.

//*-------------------------  fim  ---------------------------
//*---------------- Rotina Teste de Senha --------------------
PLIMPA()
SELE 9
USE CAD_USU INDEX CAD_USU
GOTO TOP
*SELE 2
*USE LOG_USU
*GOTO TOP
eACESSO = .F.
eVEZES  = 1
DO WHILE eVEZES <= 3
   sisUSUARIO = SPACE(10)
   sisSENHA   = SPACE(10)
   @ 11,24 TO 16,56
   SET COLOR TO W/R+,+W/W
   SET INTE ON
   PCENTRA ("Agora com Help on-line, Pressione F1.",18)
   PCENTRA ("Controle de Acesso",11)
   SET COLOR TO W/N,+W/N
   DO WHILE .T.
      @ 13,30 SAY "Usu rio : " GET sisUSUARIO PICT "@!XXXXXXXXXX"
      READ
      IF sisUSUARIO = SPACE(10)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   SET COLOR TO
   SET COLOR TO W/N,X/X
   DO WHILE .T.
      @ 14,30 SAY "Senha   : "
      @ 14,41 GET sisSENHA   PICT "@!XXXXXXXXXX"
      READ
      IF sisSENHA = SPACE(10)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   SET COLOR TO W/N,+W/R
   SET INTE ON
   SELE 9
   SEEK sisUSUARIO
   IF FOUND()
      IF sisSENHA = SENHA
         * CNPJ do Cliente
         eCNPJ        = ALLTRIM(sisCGC_CPF)
         eTAM_CNPJ    = LEN(sisCGC_CPF)
         eAUX_CNPJ    = SPACE(14)
         eAUX = 1
         DO WHILE eAUX <= eTAM_CNPJ
            IF SUBSTR(eCNPJ,eAUX,1) = "." .OR. SUBSTR(eCNPJ,eAUX,1) = "-" .OR. SUBSTR(eCNPJ,eAUX,1) = "/" .OR. SUBSTR(eCNPJ,eAUX,1) = " "
               eAUX = eAUX + 1
            ELSE
               eAUX_CNPJ = ALLTRIM(eAUX_CNPJ) + SUBSTR(eCNPJ,eAUX,1)
               eAUX = eAUX + 1
            ENDIF
         ENDDO
         eCNPJ = ALLTRIM(eAUX_CNPJ)
	 @ 23,00 SAY eCNPJ+" - Login em processo ..."
         DO CASE
         CASE eCNPJ = '02078733000174'  && SIMBOLO
            IF CTOD(sisDATA) <= CTOD('31/12/2999')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '04060621000111'  && AJ DE LIMA ME//SOLIDA
            IF CTOD(sisDATA) <= CTOD('31/12/2999')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '12407984000109'  && VIENA
            IF CTOD(sisDATA) <= CTOD('20/01/2016')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '15274945000179' && LUTECIA
            IF CTOD(sisDATA) <= CTOD('20/01/2016')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '06983112000196'  && DELLY
            IF CTOD(sisDATA) <= CTOD('20/01/2020')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '62107453000179' .OR. eCNPJ = '01723214000159' && DADO SJC e TREVILLA
            IF CTOD(sisDATA) <= CTOD('31/12/2999')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '01516542000184'  && DADO SP
            IF CTOD(sisDATA) <= CTOD('31/12/2999')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '00915210000100'  && TERRA SIMAO
            IF CTOD(sisDATA) <= CTOD('31/12/2999')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '49467137000140'  && APEMA
            IF CTOD(sisDATA) <= CTOD('20/01/2020')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '03326860000108'  && NET EMPREENDIMENTOS
            IF CTOD(sisDATA) <= CTOD('20/01/2019')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '02852392000142'  && SOLIDA DO VALE
            IF CTOD(sisDATA) <= CTOD('20/01/2020')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP
         CASE eCNPJ = '51665396000127'  && CASTOR
            IF CTOD(sisDATA) <= CTOD('20/01/2999')
               eACESSO = .T.
               EXIT
            ELSE
               SET EXCLUSIVE ON
               COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
               IF F_BLOQA()
                  REPLACE ALL SENHA WITH SPACE(10)
                  UNLOCK
               ENDIF
               SET EXCLUSIVE OFF
               eVEZES = eVEZES + 1
            ENDIF
            LOOP            
         OTHERWISE
            SET EXCLUSIVE ON
            COPY FILE CAD_USU.DBF TO CAD_LOG.DBF
            IF F_BLOQA()
               REPLACE ALL SENHA WITH SPACE(10)
               UNLOCK
            ENDIF
            SET EXCLUSIVE OFF
            eVEZES = eVEZES + 1
         ENDCASE
      ELSE
	 PLIMPA ("01")
	 PMSGR ("Senha Inv lida !",3)
	 eVEZES = eVEZES + 1
	 LOOP
      ENDIF
   ELSE
      PLIMPA ("01")
      PMSGR ("Usu rio Inv lido !",3)
      eVEZES = eVEZES + 1
      LOOP
   ENDIF
ENDDO
//*-------------------------  fim  ---------------------------
//*------------------- Chamada do Sistema -------------------
SELE 9
IF eACESSO
   MENU_PRI()
ENDIF
//*---------------  rotina finaliza sistema  ----------------
dbcloseall()
CLOSE DATABASES
SELE 9
USE CAD_USU
eDATA_S = LUPDATE()
USE
USE STATUS
IF F_BLOQR()
   REPLACE DATA_S WITH eDATA_S
ENDIF
USE
CLEAR
PTELA (sisTIT1,sisTIT2,sisTIT3,sisBAR)
SET COLOR TO R+/N+*
PCENTRA ("Copyright 1995/2020 By, All Rights Reserved ",07)
PCENTRA ("SFin - FINANCIAMENTOS",09)
SET COLOR TO B+/N+
PCENTRA ("Gitano Software",12)
SET COLOR TO GR+/N+
PCENTRA ("Rua Palmares, 116 - 12 - SJCampos - SP",14)
PCENTRA ("Cep 12235-620",15)
PCENTRA ("Fone : (012) 3307-4310",17)
PCENTRA ("Whatsapp : (12) 99664-6156",18)
PCENTRA ("WebSite : https://www.gitanosoftware.com.br/",19)
PCENTRA ("E-MAIL\SKYPE\LINKEDIN : airam1968@terra.com.br",21)
eFAZ = .F.
SET COLOR TO W/N,+W/R
SET PROCEDURE TO
CLOSE ALL
CLEAR ALL
dbcloseall()
RETURN nil
//*---------------- Funcao de Controle de Acesso -------------
FUNCTION F_ACESSO
   **** Funcao de controle de acesso a modulos
   PARAMETERS fTIPO
   eRETORNO = .F.
   SELE 9
   USE CAD_USU INDEX CAD_USU
   GOTO TOP
   SEEK sisUSUARIO
   IF &fTIPO
      eRETORNO = .T.
   ENDIF
   SELE 9
   USE
RETURN eRETORNO
//*-------------------------  fim  ---------------------------
FUNCTION F_BLOQR
   **** Funcao de Bloqueio a Registros
   SAVE SCREEN TO TELA999
   eRETORNO = .F.
   DO WHILE .T.
      PLQUA ("01")
      PMENSAG ("Aguarde, Registro em Uso Tentando ACESSO.")
      IF RLOCK()
	 eRETORNO = .T.
	 PLQUA ("01")
	 EXIT
      ELSE
	 LOOP
      ENDIF
   ENDDO
   RESTORE SCREEN FROM TELA999
RETURN eRETORNO
//*-------------------------  fim  ---------------------------
FUNCTION F_BLOQA
   **** Funcao de Bloqueio a Arquivos
   SAVE SCREEN TO TELA999
   eRETORNO = .F.
   DO WHILE .T.
      PLQUA ("01")
      PMENSAG ("Aguarde, Arquivo em Uso, Tentando ACESSO")
      IF FLOCK()
	 eRETORNO = .T.
	 PLQUA ("01")
	 EXIT
      ELSE
	 LOOP
      ENDIF
   ENDDO
   RESTORE SCREEN FROM TELA999
RETURN eRETORNO
//*-------------------------  fim  ---------------------------
//*------------------ Funcao de Log de Acesso ----------------
FUNCTION F_LACESSO
   **** Funcao de script de acesso a m½dulos
   PARAMETERS fUSUARIO,fMODULO,fACAO
   SELE 9
   USE LOG_USU 
   GOTO TOP
   APPEND BLANK
   IF F_BLOQR()
      REPLACE DATA    WITH DATE(),;
	      HORA    WITH TIME(),;
	      USUARIO WITH fUSUARIO
      REPLACE MODULO  WITH fMODULO,;
	      ACAO    WITH fACAO
      UNLOCK
   ENDIF
   SELE 9
   USE
RETURN .T.
//*-------------------------  fim  ---------------------------
********************** procedimentos ********************************
FUNCTION MOLDURA2
PARAMETERS TOPO,ESQ,DIR,BAIXO,DUPLO,TEXTO
IF PCOUNT() <6
   @ 23,10 SAY "NUMERO DE PARAMETROS INVALIDO"
   RETURN (NIL)
ENDIF
IF DUPLO
   @ TOPO,ESQ CLEAR TO DIR,BAIXO
   @ TOPO,ESQ TO DIR,BAIXO DOUBLE
ELSE
   @ TOPO,ESQ CLEAR TO DIR,BAIXO
   @ TOPO,ESQ TO DIR,BAIXO
ENDIF
IF LEN(TEXTO)>0
   TAM=BAIXO-ESQ
   NOVAPOS=(TAM-LEN(TEXTO))/2
   @ TOPO,ESQ+NOVAPOS SAY TEXTO
ENDIF
RETURN (NIL)
*
*
*          

FUNCTION PTELA
*  monta tela padrao
   PARAMETERS pTITA,pTITB,pTITC,pBAR
   pES01="Janeiro"
   pES02="Fevereiro"
   pES03="Marco"
   pES04="Abril"
   pES05="Maio"
   pES06="Junho"
   pES07="Julho"
   pES08="Agosto"
   pES09="Setembro"
   pES10="Outubro"
   pES11="Novembro"
   pES12="Dezembro"
   pESEXT=SUBSTR(sisDATA,4,2)
   pEXT=SUBSTR(sisDATA,1,2)+" de "+pES&pESEXT+" de "+SUBSTR(sisDATA,7,4)
   SET INTE ON
   @ 04,00 TO 04,79
   @ 01,00 SAY "Empresa :" GET pTITA
   @ 02,00 SAY "Sistema :" GET pTITB
   @ 02,56 SAY pEXT
   @ 22,00 TO 22,79
   CLEAR GETS
   SET INTE OFF
RETURN nil
*
*
*
*
*
*
FUNCTION PLIMPA
*  limpa a tela (os 2 quadros).
   @ 05,00 CLEAR TO 21,79
   @ 23,00 CLEAR TO 23,79
RETURN nil
*
*
*
FUNCTION PLQUA
*  limpa os quadros de acordo com o formato.
   PARAMETERS pQUADRO
   pPOS= .F.
   IF SUBSTR(pQUADRO,1,1)="1"
      @ 05,00 CLEAR TO 21,79
      pPOS= .T.
   ENDIF
   IF SUBSTR(pQUADRO,2,1)="1"
      @ 23,00 CLEAR TO 23,79
      pPOS= .T.
   ENDIF
   IF .NOT. pPOS
      PLQUA ("01")
      PMENSAG ("ERRO DO SISTEMA, INFORME O RESPONSAVEL",2,1)
	  PPARA()
      pPOS= .T.
   ENDIF
RETURN nil
*
*
*
FUNCTION PMSGR
*  mensagem rapida piscando(obs:tamanho maximo da mensagem=79).
   PARAMETERS pMSG,pPISCA
   IF EMPTY(pMSG) .OR. EMPTY(pPISCA)
      PLQUA ("01")
      PMENSAG ("ERRO DO SISTEMA, INFORME O RESPONSAVEL",2,1)
	  PPARA()
   ENDIF
   pCONT=1
   @ 23,00
   DO WHILE pCONT<=pPISCA
      SET COLOR TO W/N,W+/R
      @ 23,00 GET pMSG
      CLEAR GETS
      TONE(3900,2)
      @ 23,00
      pCONT=pCONT+1
   ENDDO
RETURN nil
*
*
*
FUNCTION PMENSAG
   * imprime uma mensagem na tela na linha de mensagens
   PARAMETERS pMENSAG
   @ 23,00
   @ 23,00 SAY pMENSAG
RETURN nil
*
*
*
FUNCTION PTEMPO
*  tempo para mensagens em tela.
   PARAMETERS pT
   pC=0
   DO WHILE pC<=pT
      pC=pC+1
   ENDDO
RETURN nil
*
*
*
FUNCTION PSENHA
   * verifica as senhas dos usuarios.
   PARAMETERS pSENHA
   PUBLIC sisSIS,sisMOD,sisNIV,sisTRUE
   FIND &pSENHA
   IF .NOT. FOUND()
      sisTRUE= .F.
   ELSE
      IF CTOD(sisDATA)>VALIDADE
         PMSGR ("SENHA DESATIVADA",1)
         sisTRUE= .F.
         DELETE
         PACK
      ELSE
         sisTRUE= .T.
         sisSIS=SIS
         sisMOD=MOD
         sisNIV=NIV
      ENDIF
   ENDIF
   USE
RETURN nil
*
*
*
FUNCTION PPERG
   * perguntas com respostas do tipo (S/N).
   PARAMETERS pPERG
   PUBLIC sisRESP
   sisRESP=SPACE(1)
   DO WHILE .NOT. sisRESP $ "SN"
	  @ 23,00 SAY pPERG GET sisRESP PICT "!"
      READ
   ENDDO
   @ 23,00 CLEAR TO 23,79
RETURN nil
*
*
*
FUNCTION POPCAO
   * seleciona opcao de menu.
   PARAMETERS pFAIXA             && faixa de possiveis opcoes.
   SET INTE ON
   sisOP=SPACE(1)
   DO WHILE .NOT. sisOP $ pFAIXA
      sisOP=SPACE(1)
      @ 23,00 SAY " Escolha sua Opcao: "
      @ 23,22 SAY "[ ]"
	  @ 23,23 GET sisOP PICT "!"
      READ
   ENDDO
   @ 23,00 CLEAR TO 23,79
   SET INTE OFF
RETURN nil
*
*
*
FUNCTION PTIT
   * centraliza titulo em tela.
   PARAMETERS pTIT
   SET COLOR TO W/N+
   @ 03,00
   @ 03,(80-LEN(pTIT))/2 SAY pTIT
   CLEAR GETS
   SET COLOR TO
   SET COLOR TO W/N,+W/R
RETURN nil
*
*
*
FUNCTION PCENTRA
   * centraliza frase na linha desejada.
   PARAMETERS pFRASE,pLIN
   @ pLIN,(80-LEN(pFRASE))/2 SAY pFRASE
RETURN nil
*
*
*
FUNCTION PPARA
   * interrompe o processamento esperando qualquer tecla.
   SET CONSOLE OFF
   WAIT
   SET CONSOLE ON
RETURN nil
*
*
*
FUNCTION PMOLDURA
   * monta moldura em tela.
   PARAMETERS pLINI,pLINF,pCOLI,pCOLF
   @ pLINI-1,pCOLI-2 TO pLINF+1,pCOLF+2 DOUBLE
   pTAM=pCOLF-pCOLI+1
   pMOLD=REPLICATE(CHR(177),pTAM)
   pC=pLINI
   DO WHILE pC<=pLINF
      @ pC,pCOLI SAY pMOLD
      pC=pC+1
   ENDDO
   @ pLINI+1,pCOLI+2 CLEAR TO pLINF-1,pCOLF-2
RETURN nil
*
*
*
FUNCTION PDESENV
   * mensagem de modulo em desenvolvimento
   PLQUA ("0111")
   PMSGR (" Aten‡Æo ! ",3)
   PMENSAG ("M¢dulo em Desenvolvimento, tecle [ENTER] p/ continuar")
   PPARA()
RETURN nil
*
FUNCTION PDESATIV
   * mensagem de modulo desativado
   PLQUA ("0111")
   PMSGR (" Aten‡Æo ! ",3)
   PMENSAG ("m¢dulo",2,1)
   PMENSAG ("Desativado",2,2)
   PMENSAG ("tecle [ENTER]",4,1)
   PMENSAG ("p/ continuar",4,2)
   PPARA()
RETURN nil
*
*
FUNCTION PDATAEXT
   * retorna data por extenso
   PARAMETERS pDATA
   PUBLIC sisEXT
   pES01="Janeiro"
   pES02="Fevereiro"
   pES03="Marco"
   pES04="Abril"
   pES05="Maio"
   pES06="Junho"
   pES07="Julho"
   pES08="Agosto"
   pES09="Setembro"
   pES10="Outubro"
   pES11="Novembro"
   pES12="Dezembro"
   pESEXT=SUBSTR(pDATA,4,2)
   sisEXT=SUBSTR(pDATA,1,2)+" de "+pES&pESEXT+" de "+SUBSTR(pDATA,7,4)
RETURN nil
*
*
*
FUNCTION PTECLA
   * procedimento para PAUSA e INTERRUPCAO da impressao.
   * para implementa-lo coloque os sequintes comandos apos o skip em
   * seu programa.
   * PTECLA
   * IF sisINTERR
   *    comandos de fechamento do programa
   *    RETURN nil
   * ENDIF
   *
   sisINTERR= .F.
   SET DEVI TO SCREEN
   PLQUA ("0100")
   @ 23,00 SAY " [ P ] - PAUSA "
   @ 23,20 SAY " [ I ] - INTERROMPE "
   pTEC1=INKEY()
   IF UPPER(CHR(pTEC1))="P"
      SET COLOR TO BGRB+*/R
      @ 23,00 SAY " [ P ] - PAUSA "
	  PPARA()
      SET COLOR TO
      @ 23,00 SAY " [ P ] - PAUSA "
   ELSE
      IF UPPER(CHR(pTEC1))="I"
         SET COLOR TO W+/GN
         @ 23,20 SAY " [ I ] - INTERROMPE "
         PTEMPO (500)
         SET COLOR TO
         @ 23,20 SAY " [ I ] - INTERROMPE "
		 PLIMPA()
         sisINTERR=.T.
         RETURN nil
      ENDIF
   ENDIF
   @ 23,00 SAY " [ P ] - PAUSA "
   @ 23,20 SAY " [ I ] - INTERROMPE "
   SET DEVI TO PRINT
RETURN nil
*
*
*
FUNCTION PIMPR
   * rotina de acusacao de erro c/impressora
   DO WHILE .NOT. ISPRINTER()
      PLQUA ("0100")
      SET COLOR TO W/N,W*+/R
      PMSGR ("Impressora Desconectada",1)
      SET COLOR TO W/N,+W/R
   ENDDO
RETURN nil
*
*
*
FUNCTION PVERIFMES
   * rotina de indicacao do periodo (MES/ANO) extenso mes
   PUBLIC pEXT
   PARAMETERS pMES,pANO
   pES01="Janeiro"
   pES02="Fevereiro"
   pES03="Marco"
   pES04="Abril"
   pES05="Maio"
   pES06="Junho"
   pES07="Julho"
   pES08="Agosto"
   pES09="Setembro"
   pES10="Outubro"
   pES11="Novembro"
   pES12="Dezembro"
   pESEXT=pMES
   pEXT=pES&pESEXT+"/"+pANO
RETURN nil
*
*
*
FUNCTION RODAPE
* procedimento de impressao de rodape especial com usuarios
PARAMETERS eTIPO,eFORM,eRELAT
PUBLIC eRODAPE_1,eRODAPE_2
IF eTIPO = 0
   SELE 10
   USE RELATOR INDEX RELATOR
   SEEK eRELAT
   IF FOUND()
      IF eFORM = 80
         eRODAPE_1=SPACE(135)
         eRODAPE_2=SPACE(135)
         eTAM=0
         eTAM=LEN(USUARIO)/6
         eVEZES=1
         eINICIO=1
         eRODAPE_1="USUARIOS : "
         eRODAPE_2="         : "
         DO WHILE eTAM >= eVEZES
            IF eVEZES <= 6
               eRODAPE_1=eRODAPE_1+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ELSE
               eRODAPE_2=eRODAPE_2+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ENDIF
            eVEZES=eVEZES+1
            eINICIO=eINICIO+10
         ENDDO
      ELSE
         eRODAPE_1=SPACE(210)
         eRODAPE_2=SPACE(210)
         eTAM=0
         eTAM=LEN(USUARIO)/10
         eVEZES=1
         eINICIO=1
         eRODAPE_1="USUARIOS : "
         eRODAPE_2="         : "
         DO WHILE eTAM >= eVEZES
            IF eVEZES <= 11
               eRODAPE_1=eRODAPE_1+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ELSE
               eRODAPE_2=eRODAPE_2+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ENDIF
            eVEZES=eVEZES+1
            eINICIO=eINICIO+10
         ENDDO
      ENDIF
   ELSE
      eRODAPE_1="**** Codigo de Relatorio nao Reconhecido ****"
      eRODAPE_2="     **** Verifique o Cadastramento ****"
   ENDIF
   USE
ELSE
   IF eFORM = 80
      @ 61,01 SAY REPLICATE("=",123)+" U.A.I."+REPLICATE("=",5)
      @ 62,01 SAY eRODAPE_1
      @ 63,01 SAY eRODAPE_2
      @ 00,00 SAY sisLETRAN+" "
      ePAG=ePAG+1
   ELSE
      @ 61,01 SAY REPLICATE("=",195)+" U.A.I."+REPLICATE("=",8)
      @ 62,01 SAY eRODAPE_1
      @ 63,01 SAY eRODAPE_2
      @ 00,00 SAY sisLETRAN+" "
      ePAG=ePAG+1
   ENDIF
ENDIF
RETURN nil
*
*
*
FUNCTION PNUMEXT
PUBLIC pNUM_EXT1,pNUM_EXT2,pNUM_EXT3
PARAMETERS pNUMERO,pTAMANHO
pN01  = "UM "
pN02  = "DOIS "
pN03  = "TRES "
pN04  = "QUATRO "
pN05  = "CINCO "
pN06  = "SEIS "
pN07  = "SETE "
pN08  = "OITO "
pN09  = "NOVE "
pN00  = " "
pN10  = "DEZ "
pN11  = "ONZE "
pN12  = "DOZE "
pN13  = "TREZE "
pN14  = "QUATORZE "
pN15  = "QUINZE "
pN16  = "DEZESSEIS "
pN17  = "DEZESETE "
pN18  = "DEZOITO "
pN19  = "DEZENOVE "
pN20  = "VINTE "
pN30  = "TRINTA "
pN40  = "QUARENTA "
pN50  = "CINQUENTA "
pN60  = "SESSENTA "
pN70  = "SETENTA "
pN80  = "OITENTA "
pN90  = "NOVENTA "
pN000 = " "
pN100 = "CENTO "
pN200 = "DUZENTOS "
pN300 = "TREZENTOS "
pN400 = "QUATROCENTOS "
pN500 = "QUINHENTOS "
pN600 = "SEISCENTOS "
pN700 = "SETECENTOS "
pN800 = "OITOCENTOS "
pN900 = "NOVECENTOS "
pMIL  = "MIL "
pMILHAO   = "MILHAO "
pMILHOES  = "MILHOES "
pMOEDA    = "REAIS "
pMOEDA1   = "REAL "
pCENTAVO  = "CENTAVO "
pCENTAVOS = "CENTAVOS "
pNUMERO1  = STR(pNUMERO,13,2)
pCENTS    = STR((pNUMERO-INT(pNUMERO))*100,2)
pNUMERO   = INT(pNUMERO)
pCONTADOR = 1
P1 = SUBSTR(pNUMERO1,2,3)
P2 = SUBSTR(pNUMERO1,5,3)
P3 = SUBSTR(pNUMERO1,8,3)
P4 = pCENTS
pTEXTO1 = ""
pTEXTO2 = ""
pTEXTO3 = ""
pTEXTO4 = ""
pLINHA1 = ""
pLINHA2 = ""
pLINHA3 = ""
IF VAL(P1) > 0
   pSTATUS1 = IIF((P1) > 1,"2","1")
ELSE
   pSTATUS1 = ""
ENDIF
DO WHILE pCONTADOR <= 4
   pITEM  = STR(pCONTADOR,1)
   pGRUPO = "P"+STR(pCONTADOR,1)
   pEXTENSO = IIF(&pGRUPO="000","",LTRIM(&pGRUPO))
   DO CASE
      CASE LEN(pEXTENSO) = 3
         IF pEXTENSO = "100"
            pTEXTO&pITEM = pTEXTO&pITEM + "CEM "
            pCONTADOR = pCONTADOR + 1
            LOOP
         ENDIF
         IF RIGHT(pEXTENSO,2) = "00"
            TAB = "pN"+SUBSTR(pEXTENSO,1,1)+"00"
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            pCONTADOR = pCONTADOR + 1
            LOOP
         ENDIF
         TAB = "pN" + LEFT(pEXTENSO,1)+"00" 
         pTEXTO&pITEM = &TAB + "E "
         pDEZENA = VAL(RIGHT(pEXTENSO,2))
         IF pDEZENA < 20
            TAB = "pN"+RIGHT(pEXTENSO,2)
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            pCONTADOR = pCONTADOR + 1
            LOOP
         ELSE
            TAB = "pN"+SUBSTR(pEXTENSO,2,1)+"0"
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            TAB = "pN0"+RIGHT(pEXTENSO,1)
            pUNIDADE = VAL(RIGHT(pEXTENSO,1))
            IF pUNIDADE > 0
               pTEXTO&pITEM = pTEXTO&pITEM + IIF(VAL(pEXTENSO) > 10,"E ","")
            ENDIF
            pTEXTO&pITEM = pTEXTO&pITEM+IIF(TAB = "pN00","",&TAB)
         ENDIF
      CASE LEN(pEXTENSO) = 2
         pDEZENA = VAL(RIGHT(pEXTENSO,2))
         IF pDEZENA < 20
            TAB = "pN"+RIGHT(pEXTENSO,2)
            pTEXTO&pITEM = pTEXTO&pITEM+&TAB
            pCONTADOR = pCONTADOR + 1
            LOOP
         ELSE
            TAB = "pN"+SUBSTR(pEXTENSO,1,1)+"0"
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            TAB = "pN0"+RIGHT(pEXTENSO,1)
            pUNIDADE1 = VAL(RIGHT(pEXTENSO,1))
            IF pUNIDADE1 > 0
               pTEXTO&pITEM = pTEXTO&pITEM + "E "
            ENDIF
            pTEXTO&pITEM = pTEXTO&pITEM+IIF(TAB="pN00","",&TAB)
         ENDIF
      CASE LEN(pEXTENSO) = 1
         TAB = "pN0"+RIGHT(pEXTENSO,1)
         pTEXTO&pITEM = pTEXTO&pITEM+IIF(TAB="pN00","",&TAB)
      OTHERWISE
         pTEXTO&pITEM = ""
   ENDCASE
   pCONTADOR = pCONTADOR + 1
ENDDO
IF VAL(P1+P2+P3) = 0 .AND. VAL(P4) <> 0 
   pFINAL = pTEXTO4+(IIF(VAL(P4)=1,pCENTAVO,pCENTAVOS))
ENDIF
pFINAL = IIF(LEN(pTEXTO1)=0,"",pTEXTO1+IIF(pSTATUS1="1",pMILHAO,pMILHOES))+IIF(LEN(pTEXTO2)=0,"",pTEXTO2+pMIL)+pTEXTO3+IIF(VAL(P2+P3)=0,"DE ","")
pFINAL = pFINAL+IIF(VAL(P1+P2+P3)=0,"",IIF(VAL(P1+P2+P3)=1,pMOEDA1,pMOEDA))
pFINAL = pFINAL+IIF(VAL(P4)=0,"",IIF(VAL(P1+P2+P3)=0,"","E ")+pTEXTO4+IIF(VAL(P4)=1,pCENTAVO,pCENTAVOS))
* Ajusta Extenso para pTAMANHO
pLINHA   = pTAMANHO
pTEXTO1  = pFINAL
pINICIAL = 1
pFINAL2  = pLINHA
pTESTE   = .T.
pITEM    = "1"
IF LEN(pTEXTO&pITEM) > pLINHA
   DO WHILE VAL(pITEM) <= 3
      DO WHILE pFINAL2 < LEN(pTEXTO&pITEM)
         pBRANCO = SUBSTR(pTEXTO&pITEM,pFINAL2,1)
         IF pBRANCO <> " "
            pFINAL2 = pFINAL2 - 1
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      pLINHA&pITEM = LEFT(pTEXTO&pITEM,pFINAL2)
      pITEMV = pITEM
      pITEM  = STR(VAL(pITEM)+1,1)
      pTEXTO&pITEM = RIGHT(pTEXTO&pITEMV,LEN(pTEXTO&PITEMV)-pFINAL2)
   ENDDO
ELSE
   pLINHA1 = pFINAL
ENDIF                        
pNUM_EXT1=pLINHA1+REPLICATE("*",pLINHA-LEN(pLINHA1))
pNUM_EXT2=pLINHA2+REPLICATE("*",pLINHA-LEN(pLINHA2))
IF pLINHA2 = pLINHA3
   pNUM_EXT3=REPLICATE("*",pLINHA)
ELSE
   pNUM_EXT3=pLINHA3+REPLICATE("*",pLINHA-LEN(pLINHA3))
ENDIF
* fim procedimento
*
*
*
FUNCTION HELP
PARAMETERS p,l,v
*SET KEY 28 TO
J_ANTERIOR:=SAVESCREEN(0,0,24,79)
CORVELHA=SETCOLOR()
SETCOLOR("W/N,+W/BG")
ARQ_ATUAL:=ALIAS()
IF .NOT. FILE("help1.dbf")
   PPERG ("ARQUIVO DE HELP INEXISTENTE. CRIA UM NOVO ?")
   IF sisRESP = "S"
      IF sisUSUARIO = "MASTER"
         RESTSCREEN(0,0,24,79,J_ANTERIOR)
         SET KEY 28 TO help()
      ENDIF
      RETURN nil
   ELSE
      MATHELP={}
      AADD(MATHELP,{"CHAVE","C",24,0})
      AADD(MATHELP,{"TOPO","N",2,0})
      AADD(MATHELP,{"ESQUERDA","N",2,0})
      AADD(MATHELP,{"BAIXO","N",2,0})
      AADD(MATHELP,{"DIREITA","N",2,0})
      AADD(MATHELP,{"TEXTOAJUDA","M",10,0})
      DBCREATE("HELP1",MATHELP)
      USE HELP1 NEW
      INDEX ON CHAVE TO HELP1
      USE
      IF LEN(ARQ_ATUAL) <>0
         SELECT( ARQ_ATUAL)
      ENDIF
   ENDIF
ENDIF
USE HELP1 INDEX HELP1 NEW
CHAVEHELP=SUBSTR(P,1,10)+SUBSTR(V,1,10)+STRZERO(L,4)
SEEK CHAVEHELP
IF FOUND()
   tl=savescreen(topo,esquerda,topo+2,direita+30)
   MOLDURA2(topo,esquerda,baixo,direita,.t.," AJUDA DE USUARIO - ESC para encerar ")
   memoedit(textoajuda,topo+1,esquerda+1,baixo-1,direita-1,.f.)
   restscreen(topo,esquerda,topo+2,direita+30,tl)
ELSE
   IF sisUSUARIO = "MASTER"
	  PLQUA("01")
	  PPERG("NAO EXISTE HELP PARA ESSE CAMPO/TELA. CRIA UM NOVO (S/N) ?")
      IF sisRESP = "N"
         RESTSCREEN(0,0,24,79,J_ANTERIOR)
         SET KEY 28 TO HELP
         USE
         IF LEN(ARQ_ATUAL)<>0
            SELECT(ARQ_ATUAL)
         ENDIF
         SET COLOR TO
         SET COLOR TO W/N,+W/BG
         RETURN nil
      ENDIF
   ELSE
      USE HELP1 INDEX HELP1 NEW
      GOTO TOP
      CHAVEHELP=SUBSTR(P,1,10)
      LOCATE FOR SUBSTR(CHAVE,1,10) = CHAVEHELP
      IF .NOT. EOF()
         tl=savescreen(topo,esquerda,topo+2,direita+30)
         MOLDURA2(topo,esquerda,baixo,direita,.t.," AJUDA DE USUARIO - ESC para encerar ")
         memoedit(textoajuda,topo+1,esquerda+1,baixo-1,direita-1,.f.)
         restscreen(topo,esquerda,topo+2,direita+30,tl)
      ELSE
         PLQUA ("01")
         PMSGR ("Acesso Negado !",3)
         RETURN nil
      ENDIF
   ENDIF
   SAVE SCREEN TO TELABAS
   DO WHILE .T.
      **STORE 0 TO TTOP,TESQ,TBAI,TDIR 
      DO WHILE .T.
         SETCURSOR(0)
         RESTORE SCREEN FROM TELABAS
         CANTO=CHR(218)
         TELATEMP=SAVESCREEN(0,0,24,79)
         TROW=INT(MAXROW() /2)
         TCOL=INT(MAXCOL() /2)
         @ TROW,TCOL SAY CANTO
         PLQUA ("01")
         PMENSAG ("POSICIONE O CURSOR ONDE SERA O CANTO SUPERIOR ")
         DO WHILE .T.
            CONTINUA=MONTATELA(INKEY(0))
            IF .NOT. CONTINUA
               EXIT
            ENDIF
            RESTORE SCREEN FROM TELABAS
            @ TROW,TCOL SAY CANTO
         ENDDO
         SAVE SCREEN TO TELAHELP
         TTOP=TROW
         TESQ=TCOL
         CANTO=CHR(188)
         TROW=TROW+2
         TCOL=TCOL+5
         @ TROW,TCOL SAY CANTO
         PLQUA ("01")
         PMENSAG ("POSICIONE O CURSOR ONDE SERA O CANTO inferior ")
         DO WHILE .T.
            CONTINUA=MONTATELA(INKEY(0))
            IF .NOT. CONTINUA
               EXIT
            ENDIF
            RESTORE SCREEN FROM TELATEMP
            QUADRO(TTOP,TESQ,TROW,TCOL,6)
         ENDDO
         SETCURSOR(1)
         STORE TROW TO TBAI
         STORE TCOL TO TDIR
         RESTORE SCREEN FROM TELAHELP
         QUADRO(TTOP,TESQ,TBAI,TCOL,6,"GR+/B")
         PPERG ("A POSICAO ESTA CORRETA (S/N) ? ")
         IF sisRESP = "N"
            RESTORE SCREEN FROM TELAHELP
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      PLQUA ("01")
      PMENSAG ("PRESSIONE CRTL+W PARA TERMINAR")
      @ TOPO,ESQUERDA TO BAIXO,DIREITA
      HELPTEMP=MEMOEDIT(TEXTOAJUDA,TTOP+1,TESQ+1,TBAI-1,TDIR-1,.T.)
      IF LASTKEY() = 27
         PLQUA ("01")
         PMENSAG ("CRIACAO DO HELP CANCELADA POR [ESC]")
		 PPARA()
         EXIT
      ENDIF
      PPERG ("Confirma o Texto (S/N) ? ")
      IF sisRESP = "N"
         PPERG ("Cancela a Criacao do HELP (S/N) ? ")
         IF sisRESP = "N"
            EXIT
         ELSE 
            LOOP
         ENDIF
      ELSE
         APPEND BLANK
         REPLACE CHAVE WITH CHAVEHELP
         REPLACE TOPO WITH TTOP,BAIXO WITH TBAI
         REPLACE ESQUERDA WITH TESQ,DIREITA WITH TDIR
         REPLACE TEXTOAJUDA WITH HELPTEMP
         USE
      ENDIF  
      EXIT
   ENDDO
ENDIF
RESTSCREEN(0,0,24,79,J_ANTERIOR)
USE
IF LEN(ARQ_ATUAL) <>0
        SELECT(ARQ_ATUAL)
ENDIF
SETCOLOR(CORVELHA)
SET KEY 28 TO HELP
RETURN nil
*
*
*
FUNCTION MONTATELA
PARAMETERS CURSOR
DO CASE
   CASE CURSOR=5
      IF TROW-1 >0
         TROW=TROW-1
      ENDIF
   CASE CURSOR=24
      IF TROW+1 <24
         TROW=TROW+1
      ENDIF
   CASE CURSOR=4
      IF TCOL+1 <79
         TCOL=TCOL+1
      ENDIF
   CASE CURSOR=19
      IF TCOL-1 >0
         TCOL=TCOL-1
      ENDIF
   CASE CURSOR = 13 .OR. CURSOR = 27
	  RETURN (.F.)
ENDCASE
RETURN (.T.)
*
*
*
FUNCTION QUADRO
PARAMETERS LS,CS,LI,CI,TIPO,COR,CHEIO
LOCAL Q[9]
CORBOX=COR
IF TIPO =NIL
   TIPO=1
ENDIF
IF CHEIO=NIL
   CHEIO=.F.
ENDIF
Q[1]:=CHR(201)+CHR(205)+CHR(187)+CHR(186)+CHR(188)+CHR(205)+CHR(200)+CHR(186)
Q[2]:=CHR(218)+CHR(196)+CHR(191)+CHR(179)+CHR(217)+CHR(196)+CHR(192)+CHR(179)
Q[3]:=CHR(213)+CHR(205)+CHR(184)+CHR(179)+CHR(190)+CHR(205)+CHR(212)+CHR(179)
Q[4]:=CHR(214)+CHR(196)+CHR(183)+CHR(186)+CHR(189)+CHR(196)+CHR(211)+CHR(186)
Q[5]:=CHR(220)+CHR(220)+CHR(220)+CHR(219)+CHR(219)+CHR(220)+CHR(219)+CHR(219)
Q[6]:=CHR(218)+CHR(196)+CHR(183)+CHR(186)+CHR(188)+CHR(205)+CHR(212)+CHR(179)
Q[7]:=CHR(201)+CHR(205)+CHR(184)+CHR(179)+CHR(217)+CHR(196)+CHR(211)+CHR(186)
IF VALTYPE(TIPO)="C"
   Q[9]=REPL(TIPO,8)
   TIPO=9
ENDIF
IF (TIPO >=176 .AND. TIPO <=178) .OR. TIPO=219
   IF CHEIO
      Q[8]=REPLICATE(CHR(TIPO),9)
   ELSE
      Q[8]=REPLICATE(CHR(TIPO),8)
   ENDIF
   TIPO=8
ENDIF
IF TIPO <>8 .AND. CHEIO
   Q[TIPO]=Q[TIPO]=CHR(32)
ENDIF
IF COR<>NIL
   @ LS,CS,LI,CI BOX Q[TIPO]
ELSE
   @ LS,CS,LI,CI BOX Q[TIPO]
ENDIF
*
*
*
FUNCTION TESTACGC
PARAMETERS pCGC
pCGC2    = ALLTRIM(pCGC)
pTAM_CGC = LEN(pCGC2)
pAUX_CGC = SPACE(14)
pAUX = 1
DO WHILE pAUX <= pTAM_CGC
   IF SUBSTR(pCGC2,pAUX,1) = "." .OR. SUBSTR(pCGC2,pAUX,1) = "-" .OR. SUBSTR(pCGC2,pAUX,1) = "/" .OR. SUBSTR(pCGC2,pAUX,1) = " ";
       .OR. SUBSTR(pCGC2,pAUX,1) = "\" .OR. SUBSTR(pCGC2,pAUX,1) = "," .OR. SUBSTR(pCGC2,pAUX,1) = ";"
      pAUX = pAUX + 1
   ELSE
      pAUX_CGC = ALLTRIM(pAUX_CGC) + SUBSTR(pCGC2,pAUX,1)
      pAUX = pAUX + 1
   ENDIF
ENDDO
pCGC = ALLTRIM(pAUX_CGC)
pCGC2=SPACE(14)
pNUM1=SUBSTR(pCGC,1,1)+SUBSTR(pCGC,3,1)+SUBSTR(pCGC,5,1)+SUBSTR(pCGC,7,1)
pNUM1=ALLTRIM(STR(VAL(pNUM1)*2.5))
pNUM1=VAL(SUBSTR(pNUM1,1,1))+VAL(SUBSTR(pNUM1,2,1))+VAL(SUBSTR(pNUM1,3,1))+VAL(SUBSTR(pNUM1,4,1))+VAL(SUBSTR(pNUM1,5,1))
pNUM2=ALLTRIM(SUBSTR(pCGC2,2,1)+SUBSTR(pCGC2,4,1)+SUBSTR(pCGC2,6,1)+SUBSTR(pCGC2,8,1))
pNUM2=VAL(SUBSTR(pNUM2,1,1))+VAL(SUBSTR(pNUM2,2,1))+VAL(SUBSTR(pNUM2,3,1))+VAL(SUBSTR(pNUM2,4,1))
pNUM3=pNUM1+pNUM2
IF pNUM3 <> INT(pNUM3/10)*10
   RETURN .F.
ELSE
   RETURN .T.
ENDIF
*
*
*
FUNCTION TESTACPF
PARAMETERS pCPF
pCPF2    = ALLTRIM(pCPF)
pTAM_CPF = LEN(pCPF2)
pAUX_CPF = SPACE(14)
pAUX = 1
DO WHILE pAUX <= pTAM_CPF
   IF SUBSTR(pCPF2,pAUX,1) = "." .OR. SUBSTR(pCPF2,pAUX,1) = "-" .OR. SUBSTR(pCPF2,pAUX,1) = "/" .OR. SUBSTR(pCPF2,pAUX,1) = " ";
       .OR. SUBSTR(pCPF2,pAUX,1) = "\" .OR. SUBSTR(pCPF2,pAUX,1) = "," .OR. SUBSTR(pCPF2,pAUX,1) = ";"
      pAUX = pAUX + 1
   ELSE
      pAUX_CPF = ALLTRIM(pAUX_CPF) + SUBSTR(pCPF2,pAUX,1)
      pAUX = pAUX + 1
   ENDIF
ENDDO
pCPF = ALLTRIM(pAUX_CPF)
pCONTADOR=1
DO WHILE pCONTADOR <= 9
   D = "0"+STR(pCONTADOR,1)
   D2&D = VAL(SUBSTR(pCPF,pCONTADOR,1))
   pCONTADOR = pCONTADOR + 1
ENDDO
DF4 = 10*D201+9*D202+8*D203+7*D204+6*D205+5*D206+4*D207+3*D208+2*D209
DF5 = DF4 / 11
DF6 = INT(DF5)*11
RESTO1 = DF4 - DF6
IF RESTO1 = 0 .OR. RESTO1 = 1
   PRIDIG = 0
ELSE
   PRIDIG = 11 - RESTO1
ENDIF
pCONTADOR = 1
DO WHILE pCONTADOR <= 9
   D = "0"+STR(pCONTADOR,1)
   D2&D = VAL(SUBSTR(pCPF,pCONTADOR,1))
   pCONTADOR = pCONTADOR + 1
ENDDO
DF4=11*D201+10*D202+9*D203+8*D204+7*D205+6*D206+5*D207+4*D208+3*D209+2*PRIDIG
DF5 = DF4 / 11
DF6 = INT(DF5) * 11
RESTO1 = DF4-DF6
IF RESTO1 = 0 .OR. RESTO1 = 1
   SEGDIG = 0
ELSE
   SEGDIG = 11 - RESTO1
ENDIF
PRIDIG = STR(PRIDIG,1)
SEGDIG = STR(SEGDIG,1)
IF PRIDIG <> SUBSTR(pCPF,10,1) .OR. SEGDIG <> SUBSTR(pCPF,11,1)
   RETURN .F.
ELSE
   RETURN .T.
ENDIF
*
*
*
******************************  fim  *****************************