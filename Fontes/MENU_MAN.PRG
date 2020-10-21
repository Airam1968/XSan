*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_MAN.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO MANUTENCAO DO SISTEMA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_MAN()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU MANUTENÄ«O")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 19,59
   @ 09,40 TO 19,59
   @ 10,41 PROMPT " 1   Reindexaá∆o  " MESSAGE " Reorganizaá∆o dos Arquivos de ÷ndice      "
   @ 11,41 PROMPT " 2  Cad. Usu†rios " MESSAGE " Cadastramento de Usu†rios do Sistema      "
   @ 12,41 PROMPT " 3     Status     " MESSAGE " Dados Gerais do Sistema                   "
   @ 13,41 PROMPT " 4   Parametros   " MESSAGE " Parametros Gerais do Sistema Master       "
   @ 14,41 PROMPT " 5    Especiais   " MESSAGE " Acesso apenas para o Usuario Master       "
   @ 15,41 PROMPT " 6   Licenáa Uso  " MESSAGE " Licenáa de Uso para Usu†rio Final - CLUF  "
   @ 16,41 PROMPT " 7 Imp./Exportaá∆o" MESSAGE " Menu de Importaá‰es e Exportaá‰es de Dados"
   @ 17,41 PROMPT " 8 Bloco de Notas " MESSAGE " Bloco de Notas do Sistema                 "   
   @ 18,41 PROMPT " Esc     Fim      " MESSAGE " Fim de Operaá∆o do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
	SAVE SCREEN TO TELA04
	PLQUA("01")
	PPERG("Confirma (S/N) ? ")
	IF sisRESP = "S"
	   @ 12,10 CLEAR TO 20,70
           SET COLOR TO GR+/N+
	   @ 12,10 TO 20,70 DOUBLE
           SET COLOR TO R+/N+*
	   PCENTRA(" ALERTA ! ",12)
           SET COLOR TO R+/N+
	   PCENTRA("Para efetuar esta operaá∆o e necess†rio  que  todos",14)
	   PCENTRA("os usu†rios n∆o estejam em uso do sistema, solicite",15)
	   PCENTRA("a todos que saiam do Sistema antes de  CONFIRMAR  a",16)
           PCENTRA("reconstruá∆o dos arquivos de indice.               ",17)
	   PCENTRA("                                        Obrigado   ",19)
           SET COLOR TO W/N,+W/R
	   PLQUA("01")
	   PMENSAG("Pressione Qualquer Tecla Para Iniciar o Processo ...")
	   PPARA()
	   eFAZ = .T.
	   REINDEXA()
	ENDIF
	PLQUA("01")
	RESTORE SCREEN FROM TELA04
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_09")
	   SAVE SCREEN TO TELA03
	   PTIT("MANUTENCAO DE USUµRIOS")
	   PLQUA("01")
	   PPERG("Confirma (S/N) ? ")
	   IF sisRESP = "S"
	      SELE 1
	      USE CAD_USU INDEX CAD_USU
	      GOTO TOP
	      SELE 2
	      USE STATUS
	      GOTO TOP
	      SELE 1
	      DO PLIMPA
	      SET COLOR TO W/N,+W/R
              DECLARE CAMPO[1],PIC[1],CAB[1],LENS[1]
	      AFIELDS(CAMPO,.T.,LENS)
	      PIC[1] ="@!XXXXXXXXXX"
	      CAB[1] ="USUµRIO"
              @ 5,30 TO 21,50 DOUBLE
	      PLQUA("01")
	      SET COLOR TO
	      SET COLOR TO W/N+*
	      @ 23,00 SAY "[ENTER]"
	      @ 23,21 SAY "[ESC]"
	      @ 23,37 SAY "[ESPAÄO]"
	      @ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
	      SET COLOR TO
	      SET COLOR TO W/N,+W/R
	      @ 23,07 SAY "Para Alterar"
	      @ 23,26 SAY "Para Sair"
	      @ 23,45 SAY "Para Incluir"
	      @ 23,65 SAY "Movimentaá∆o"
              DBEDIT(06,31,21,49,CAMPO,"F_CAD_USU",PIC,CAB,CHR(196),CHR(179),CHR(205))
	   ENDIF
	   CLOSE ALL
	   RESTORE SCREEN FROM TELA03
	ELSE
	   PLQUA("01")
	   PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CADASTRO DE USUARIOS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_AL_09")
           PLQUA("01")
           PPERG("Confirma (S/N) ? ")
           IF sisRESP = "S"
              SAVE SCREEN TO TELA03
              PLQUA("11")
              PTIT("STATUS DO SISTEMA")
              SELE 1
              USE STATUS
              GOTO TOP
              IF F_BLOQR()
                 @ 05,02 SAY "DADOS DA EMPRESA"
                 @ 07,02 SAY "Empresa  : " GET EMPRESA
                 @ 08,02 SAY "CGC      : " GET CGC_CPF 
                 @ 08,40 SAY "Inscricao: " GET INSCRICAO
                 @ 09,02 SAY "Endereáo : " GET ENDERECO
                 @ 10,02 SAY "Bairro   : " GET BAIRRO
                 @ 11,02 SAY "Cidade   : " GET CIDADE
                 @ 12,02 SAY "Estado   : " GET ESTADO
                 @ 12,30 SAY "Contato  : " GET CONTATO
                 @ 13,02 SAY "CEP      : " GET CEP  PICT "@R 99999-999"
                 @ 13,30 SAY "E-Mail   : " GET EMAIL
                 @ 14,02 SAY "Telefone : " GET TEL1 PICT "@R (XX) XXXXX-XXXXX"
                 @ 15,02 SAY "Telefone : " GET TEL2 PICT "@R (XX) XXXXX-XXXXX"
                 READ
                 sisTIT1=ALLTRIM(EMPRESA)    
                 sisCGC_CPF=CGC_CPF                  
                 @ 16,02 SAY "DADOS DO SOFTWARE"
                 @ 18,02 SAY "Sistema  : " GET SISTEMA
                 @ 19,02 SAY "VersÉo   : " GET VERSAO
                 @ 20,02 SAY "Depart.  : " GET DEPTO
                 CLEAR GETS
                 @ 21,02 SAY "Moeda    : " GET MOEDA
                 READ
                 PMENSAG("Pressione Qualquer Tecla Para Continuar ")
                 PPARA()
                 UNLOCK
              ENDIF
              PMENSAG("Pressione Qualquer Tecla Para Continuar ")
              PPARA()
              CLOSE DATABASES
              RESTORE SCREEN FROM TELA03
           ENDIF
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_AL_09")
           PLQUA("01")
           PPERG("Confirma (S/N) ? ")
           IF sisRESP = "S"
              SAVE SCREEN TO TELA03
              PLQUA("11")
              PTIT("PARAMETROS DO SISTEMA")
              SELE 1
              USE STATUS
              GOTO TOP
              IF F_BLOQR()
                 @ 07,02 SAY "C¢digo Proximo Cliente            : " GET NUM_CLIE PICT "@R 999,999,999"
                 @ 08,02 SAY "C¢digo Pr¢ximo Recibo             : " GET NUM_RECI PICT "@R 999,999,999"
                 @ 09,02 SAY "Pr¢ximo Laná. Contas a Pagar      : " GET LANC_CPG PICT "@R 999,999,999"
                 @ 10,02 SAY "Pr¢ximo Laná. Contas a Receber    : " GET LANC_CRE PICT "@R 999,999,999"
                 @ 11,02 SAY "C¢digo Proximo Fornecedor CPagar  : " GET FORN_CPG PICT "@R 999,999,999"
                 @ 12,02 SAY "C¢digo Proximo Cliente CReceber   : " GET CLIE_CPG PICT "@R 999,999,999"
                 @ 13,02 SAY "Cad. Fornecedor Ord. Alfabetica   : " GET ORDE_FORN
                 @ 14,02 SAY "Cad. Cliente Ord. Alfabetica      : " GET ORDE_CLIE
                 @ 15,02 SAY "Classe Despesas Ord. Alfabetica   : " GET ORDE_CLAD
                 @ 16,02 SAY "Classe Receitas Ord. Alfabetica   : " GET ORDE_CLAR
                 @ 17,02 SAY "Multa Por Atraso Pagamento a.m.   : " GET MULTA    PICT "@R 99.999"
                 @ 18,02 SAY "Produtos Ordem Alfabetica         : " GET ORDE_CLAS
                 @ 19,02 SAY "Comiss∆o de Permanencia a.m.      : " GET COM_PERM PICT "@R 99.999"
                 @ 20,02 SAY "Taxa de Desconto a.m.             : " GET DESCONTO PICT "@R 99.999"
                 READ
                 PLQUA("01")
                 PMENSAG("Pressione Qualquer Tecla Para Continuar ")
                 PPARA()
                 PLQUA("11")                 
                 @ 07,02 SAY "Corrige Parcela Paga              : " GET CORR_PGTO
                 @ 08,02 SAY "Corrige Parcela Atrasada          : " GET CORR_ATRA
                 @ 09,02 SAY "Corr.de Parcelas com No. Boleto   : " GET CORR_BOLE
                 @ 10,02 SAY "Baixa em Parcelas Inativas        : " GET BAIXA_ATIV                 
                 @ 11,02 SAY "Residuo Integral na 13a. Parcela  : " GET RESIDUO_I
                 @ 12,02 SAY "Residuo Integral em Separado      : " GET RESI_SEPA
                 @ 13,02 SAY "Pro-Rata no Pagamento da Parcela  : " GET PRO_RATA
                 @ 14,02 SAY "Permite ANATOCISMO nos Juros      : " GET ANATOCISMO
                 @ 15,02 SAY "Bloco de Notas do Cliente         : " GET BLOCO_CLIE

                 @ 17,02 SAY "Drive para Geraá∆o de Relat¢rios  : " GET DRIVE_REL
                 @ 18,02 SAY "Pasta para Relat¢rios (\exemplo\) : " GET PASTA_REL
                
                 @ 20,02 SAY "Controle de Manutená∆o Indices    : " GET CONTROLE PICT "@R 99,999,999"
                 READ
				 PLQUA("01")
                 PMENSAG("Pressione Qualquer Tecla Para Continuar ")
                 PPARA()
                 PLQUA("11")  
                 @ 07,02 SAY "Drive Importaá∆o Contas a Pagar   : " GET D_IMPCP PICTURE "@!XXX"
                 @ 08,02 SAY "Pasta para Importaá∆o (\exemplo\) : " GET P_IMPCP	
                 @ 09,02 SAY "Extená∆o para Importaá∆o          : " GET E_IMPCP	PICTURE "@!XXX"		
				 
                 @ 11,02 SAY "Drive Importaá∆o Contas a Receber : " GET D_IMPCR PICTURE "@!XXX"
                 @ 12,02 SAY "Pasta para Importaá∆o (\exemplo\) : " GET P_IMPCR	
                 @ 13,02 SAY "Extená∆o para Importaá∆o          : " GET E_IMPCR	PICTURE "@!XXX"		
				 READ
                 UNLOCK
              ENDIF
              PMENSAG("Pressione Qualquer Tecla Para Continuar ")
              PPARA()
              CLOSE DATABASES
              RESTORE SCREEN FROM TELA03
           ENDIF
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CA_09")
           MENU_SEG()
	ELSE
	   PLQUA("01")
	   PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MENU LOG DE SEGURANCA","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 6
        SAVE SCREEN TO T_PROC2
        @ 07,01 CLEAR TO 21,79
        @ 07,01 TO       21,79
        PCENTRA(" Licenáa de Uso para Usu†rio Final - CLUF",07)       
        SET CURSOR ON
        eOLD = MEMOREAD("licenca.txt")
        eNEW = MEMOEDIT(eOLD,08,02,20,78,.T.,"F_LICENCA",60,10,1,1)
        SET CURSOR OFF
        PPARA()
        RESTORE SCREEN FROM T_PROC2
     CASE sisOP = 7
        MENU_IME()
     CASE sisOP = 8
        SAVE SCREEN TO T_PROC2
        @ 07,01 CLEAR TO 21,79
        @ 07,01 TO       21,79
        PCENTRA(" Quadro Bloco de Notas do Sistema",07)       
        SET CURSOR ON
        eOLD = MEMOREAD("SIS_OBS.txt")
        eNEW = MEMOEDIT(eOLD,08,02,20,78,.T.,"F_LICENCA",60,10,1,1)
        SET CURSOR OFF
        PPARA()
        RESTORE SCREEN FROM T_PROC2     
     CASE LASTKEY() = 27
	CLOSE DATABASES
	RESTORE SCREEN FROM TELA02
	RETURN NIL
  ENDCASE
ENDDO
*************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_CAD_USU
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 32
	 SAVE SCREEN
	 SET CURSOR ON
	 PLQUA("01")
	 PPERG("Confirma Inclusao (S/N) ? ")
	 IF sisRESP = "S"
	    DO WHILE .T.
		   @ 09,15 CLEAR TO 20,65
		   @ 09,15 TO 20,65 DOUBLE
		   PCENTRA("Dados do Usu†rio",10)
	       eUSUARIO   = SPACE(10)
	       eSENHA     = SPACE(10)
               eDI_CA_01  = .F.
               eDI_AL_01  = .F.
               eDI_EX_01  = .F.
               eDI_CO_01  = .F.
               eDI_RE_01  = .F.
               eDI_CA_02  = .F.
               eDI_AL_02  = .F.
               eDI_EX_02  = .F.
               eDI_CO_02  = .F.
               eDI_RE_02  = .F.
               eDI_CA_03  = .F.
               eDI_AL_03  = .F.
               eDI_EX_03  = .F.
               eDI_CO_03  = .F.
               eDI_RE_03  = .F.
               eDI_CA_04  = .F.
               eDI_AL_04  = .F.
               eDI_EX_04  = .F.
               eDI_CO_04  = .F.
               eDI_RE_04  = .F.
               eDI_CA_05  = .F.
               eDI_AL_05  = .F.
               eDI_EX_05  = .F.
               eDI_CO_05  = .F.
               eDI_RE_05  = .F.
               eDI_CA_06  = .F.
               eDI_AL_06  = .F.
               eDI_EX_06  = .F.
               eDI_CO_06  = .F.
               eDI_RE_06  = .F.
               eDI_CA_07  = .F.
               eDI_AL_07  = .F.
               eDI_EX_07  = .F.
               eDI_CO_07  = .F.
               eDI_RE_07  = .F.
               eDI_CA_08  = .F.
               eDI_AL_08  = .F.
               eDI_EX_08  = .F.
               eDI_CO_08  = .F.
               eDI_RE_08  = .F.
               eDI_CA_09  = .F.
               eDI_AL_09  = .F.
               eDI_EX_09  = .F.
               eDI_CO_09  = .F.
               eDI_RE_09  = .F.
               DO WHILE .T.
                  @ 12,27 SAY "Usu†rio  : " GET eUSUARIO   PICT "@!XXXXXXXXXXX"
                  READ
                  IF eUSUARIO <> SPACE(10)
					SELE 1
					GOTO TOP
					SEEK eUSUARIO
					IF FOUND()
						PLQUA("01")
						PMSGR ("Nome de Usu†rio j· Existe !",3)
					ELSE
					   EXIT
					ENDIF
				  ELSE
					RESTORE SCREEN
					RETURN RET
                  ENDIF
               ENDDO
               CLEAR GETS
               SET COLOR TO
               SET COLOR TO W/N,X/X               
               DO WHILE .T.
                  @ 13,27 SAY "Senha    : " GET eSENHA     PICT "@!XXXXXXXXXXX"
                  READ
                  IF eSENHA <> SPACE(10)
                     EXIT
                  ENDIF
               ENDDO  
               SET COLOR TO W/N,+W/R
               SET INTE ON 
               PCENTRA("Direitos de Acesso - Modulo Clientes",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_01 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_01 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_01 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_01 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_01 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Produto",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_02 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_02 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_02 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_02 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_02 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Vendas Pagamentos",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_03 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_03 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_03 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_03 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_03 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Relat¢rios",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_04 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_04 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_04 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_04 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_04 PICT "@LX"
	       READ	       
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Contas a Pagar",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_05 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_05 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_05 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_05 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_05 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Contas a Receber",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_06 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_06 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_06 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_06 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_06 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Caixas e Bancos",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_07 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_07 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_07 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_07 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_07 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Fluxo de Caixa",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_08 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_08 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_08 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_08 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_08 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Manutená∆o",14)
               @ 15,27 SAY "Cadastrar : " GET eDI_CA_09 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET eDI_AL_09 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET eDI_EX_09 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET eDI_CO_09 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET eDI_RE_09 PICT "@LX"
	       READ
	       PLQUA("01")
	       PPERG("Confirma (S/N) ? ")
	       IF sisRESP = "S"
		  SELE 2
		  IF F_BLOQR()
		     REPLACE DATA_S WITH DATE()
		     UNLOCK
		  ENDIF
		  SELE 1
		  APPEND BLANK
		  IF F_BLOQR()
		     REPLACE USUARIO    WITH eUSUARIO,;
			     SENHA      WITH eSENHA
                     REPLACE DI_CA_01   WITH eDI_CA_01,;
                             DI_AL_01   WITH eDI_AL_01,;
                             DI_EX_01   WITH eDI_EX_01
                     REPLACE DI_CO_01   WITH eDI_CO_01,;
                             DI_RE_01   WITH eDI_RE_01
                     REPLACE DI_CA_02   WITH eDI_CA_02,;
                             DI_AL_02   WITH eDI_AL_02,;
                             DI_EX_02   WITH eDI_EX_02
                     REPLACE DI_CO_02   WITH eDI_CO_02,;
                             DI_RE_02   WITH eDI_RE_02
                     REPLACE DI_CA_03   WITH eDI_CA_03,;
                             DI_AL_03   WITH eDI_AL_03,;
                             DI_EX_03   WITH eDI_EX_03
                     REPLACE DI_CO_03   WITH eDI_CO_03,;
                             DI_RE_03   WITH eDI_RE_03
                     REPLACE DI_CA_04   WITH eDI_CA_04,;
                             DI_AL_04   WITH eDI_AL_04,;
                             DI_EX_04   WITH eDI_EX_04
                     REPLACE DI_CO_04   WITH eDI_CO_04,;
                             DI_RE_04   WITH eDI_RE_04
                     REPLACE DI_CA_05   WITH eDI_CA_05,;
                             DI_AL_05   WITH eDI_AL_05,;
                             DI_EX_05   WITH eDI_EX_05
                     REPLACE DI_CO_05   WITH eDI_CO_05,;
                             DI_RE_05   WITH eDI_RE_05
                     REPLACE DI_CA_06   WITH eDI_CA_06,;
                             DI_AL_06   WITH eDI_AL_06,;
                             DI_EX_06   WITH eDI_EX_06
                     REPLACE DI_CO_06   WITH eDI_CO_06,;
                             DI_RE_06   WITH eDI_RE_06
                     REPLACE DI_CA_07   WITH eDI_CA_07,;
                             DI_AL_07   WITH eDI_AL_07,;
                             DI_EX_07   WITH eDI_EX_07
                     REPLACE DI_CO_07   WITH eDI_CO_07,;
                             DI_RE_07   WITH eDI_RE_07
                     REPLACE DI_CA_08   WITH eDI_CA_08,;
                             DI_AL_08   WITH eDI_AL_08,;
                             DI_EX_08   WITH eDI_EX_08
                     REPLACE DI_CO_08   WITH eDI_CO_08,;
                             DI_RE_08   WITH eDI_RE_08
                     REPLACE DI_CA_09   WITH eDI_CA_09,;
                             DI_AL_09   WITH eDI_AL_09,;
                             DI_EX_09   WITH eDI_EX_09
                     REPLACE DI_CO_09   WITH eDI_CO_09,;
                             DI_RE_09   WITH eDI_RE_09
		     UNLOCK
		  ENDIF
		  EXIT
	       ELSE
		  EXIT
	       ENDIF
	    ENDDO
	 ENDIF
	 SET CURSOR OFF
	 RESTORE SCREEN
      CASE KEY == 13
	 SAVE SCREEN
	 SET CURSOR ON
	 PLQUA("01")
         PPERG("Confirma Alteraá∆o (S/N) ? ")
	 IF sisRESP = "S"
	    DO WHILE .T.
               @ 09,15 CLEAR TO 20,65
               @ 09,15 TO 20,65 DOUBLE
               PCENTRA("Dados do Usu†rio",10)
               IF F_BLOQR()
	       DO WHILE .T.
		  @ 12,27 SAY "Usu†rio  : " GET USUARIO   PICT "@!XXXXXXXXXXX"
		  READ
		  IF USUARIO <> SPACE(10) 
		     EXIT
		  ENDIF
	       ENDDO
	       CLEAR GETS
	       SET COLOR TO
	       SET COLOR TO W/N,X/X               
	       DO WHILE .T.
		  @ 13,27 SAY "Senha    : " GET SENHA     PICT "@!XXXXXXXXXXX"
		  READ
		  IF SENHA <> SPACE(10)
		     EXIT
		  ENDIF
	       ENDDO  
	       SET COLOR TO W/N,+W/R
	       SET INTE ON 	       
               PCENTRA("Direitos de Acesso - Modulo Clientes",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_01 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_01 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_01 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_01 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_01 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Produtos",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_02 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_02 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_02 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_02 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_02 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Venda/Pagamentos",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_03 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_03 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_03 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_03 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_03 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Relat¢rios",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_04 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_04 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_04 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_04 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_04 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Contas a Pagar",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_05 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_05 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_05 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_05 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_05 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Contas a Receber",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_06 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_06 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_06 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_06 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_06 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Caixas e Bancos",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_07 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_07 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_07 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_07 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_07 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Fluxo de Caixa",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_08 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_08 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_08 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_08 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_08 PICT "@LX"
	       READ
               @ 14,16 CLEAR TO 19,64
               PCENTRA("Direitos de Acesso - Modulo Manutená∆o",14)
               @ 15,27 SAY "Cadastrar : " GET DI_CA_09 PICT "@LX"
               @ 16,27 SAY "Alterar   : " GET DI_AL_09 PICT "@LX"
               @ 17,27 SAY "Excluir   : " GET DI_EX_09 PICT "@LX"
               @ 18,27 SAY "Consultar : " GET DI_CO_09 PICT "@LX"
               @ 19,27 SAY "Relat¢rio : " GET DI_RE_09 PICT "@LX"
	       READ
               UNLOCK
               ENDIF
               EXIT
	    ENDDO
	 ENDIF
	 SET CURSOR OFF
	 RESTORE SCREEN
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
************************ funcao *********************************
FUNCTION F_LICENCA
PARAMETERS mode, line, column
PRIVATE keypress, ret2
RET2 = 0
KEYPRESS = LASTKEY()
IF MODE == 1 .OR. MODE == 2
   DO CASE
      CASE KEYPRESS == 273
         RET2 = 34
      CASE KEYPRESS == -4
         RET2 = 1
      CASE KEYPRESS == -5
         RET2 = 6
      CASE KEYPRESS == -8
         RET2 = 25
      CASE KEYPRESS == 301
         RET2 = 23
   ENDCASE
ENDIF
RETURN RET2
*************************  fim  *********************************

