*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MAL_CART.PRG
* Data        : 01/01/99
* Objetivo    : PROGRAMA DE MALA DIRETA CARTAS
* PRG"s       :
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MAL_CART()
PLQUA("01")
PPERG("Confirma ? (S/N)")
IF sisRESP="N"
   RETURN nil
ENDIF
PLQUA("01")
DO WHILE .T.
   eCOPIA = 0
   @ 23,00 SAY "Informe o Numero de Copias por Cliente : " GET eCOPIA PICT "99"
   READ
   IF eCOPIA <> 0
      EXIT
   ENDIF
ENDDO
PLQUA("01")
PPERG ("Deseja Emitir as Cartas Tendo como Remetente a Empresa (S/N) ? ")
IF sisRESP = "S"
   eREMETENTE = .T.
ELSE 
   eREMETENTE = .F.
   PLQUA("01")
   PPERG("Deseja Como Remetente o Proprio Cliente (S/N) ? ")
   IF sisRESP = "S"
      eNOME_REME = SPACE(40)
      * Remetente vazio imprime o nome do cliente
   ELSE
      DO WHILE .T.
         eNOME_REME = SPACE(40)
         @ 23,00 SAY "Remetente : " GET eNOME_REME
         READ
         IF eNOME_REME <> SPACE(40)
            EXIT
         ENDIF
      ENDDO
   ENDIF
ENDIF
PLQUA("01")
PMENSAG("Aguarde, Preparando Nomes Selecionados ...")
SELE 3
USE STATUS
GOTO TOP
eCID_EMPRESA = CIDADE
SELE 2
USE CAR_PADR INDEX CAR_PADR
SET FILTER TO SELECAO = "X"
GOTO TOP
SELE 1
USE CONTATO INDE CONTATO,NUM_CLIE
SET FILTER TO SELECAO = "X"
GOTO TOP
IF EOF()
   PLQUA("01")
   PMENSAG("N∆o H† clientes selecionados, Tecle [ENTER] para finalizar")
   PPARA()
ENDIF
PLQUA("01")
PMENSAG("Posicione o Papel na impressora, Tecle [ENTER] para comecar")
PPARA()
DO PIMPR
PLQUA("01")
PMENSAG("Aguarde ...")
SET DEVICE TO PRINT
eAUX = 1
SELE 1
GOTO TOP
DO WHILE .NOT. EOF()
   SET DEVICE TO PRINT
   eCONTA = 1
   DO WHILE eCONTA <= eCOPIA
      * Inicio da Impressao
      @ 05,50 SAY ALLTRIM(eCID_EMPRESA)+"," 
      @ 06,50 SAY pEXT+"."
      eTIT = SPACE(20)
      SELE 1
      IF SEXO = "M"
         eTIT = "Ilmo Sr."
      ELSE
         IF SEXO = "F"
            eTIT = "Ilma Sra."
         ELSE
            eTIT = "A"
         ENDIF
      ENDIF
      @ 12,05 SAY eTIT
      @ 14,05 SAY CLIENTE
      IF OPCAO = "R"
         @ 15,05 SAY END_R
         @ 16,05 SAY ALLTRIM(BAIRRO_R)+" - "+ALLTRIM(CIDADE_R)+" - "+ALLTRIM(ESTADO_R)
         IF CEP_R <> SPACE(8)
            @ 17,05 SAY "CEP "+ALLTRIM(CEP_R) PICT "@R XXXXXXXXX-XXX"
         ENDIF
      ELSE
         @ 15,05 SAY END_C
         @ 16,05 SAY ALLTRIM(BAIRRO_C)+" - "+ALLTRIM(CIDADE_C)+" - "+ALLTRIM(ESTADO_C)
         IF CEP_C <> SPACE(8)
            @ 17,05 SAY "CEP "+ALLTRIM(CEP_C) PICT "@R XXXXXXXXX-XXX"
         ENDIF
      ENDIF
      @ 18,05 SAY "Cod.Empresa :"+IDENTIF
      SELE 2
      @ 21,05 SAY LINHA_1
      @ 22,05 SAY LINHA_2
      @ 23,05 SAY LINHA_3
      @ 24,05 SAY LINHA_4
      @ 25,05 SAY LINHA_5
      @ 26,05 SAY LINHA_6
      @ 27,05 SAY LINHA_7
      @ 28,05 SAY LINHA_8
      @ 29,05 SAY LINHA_9
      @ 30,05 SAY LINHA_10
      @ 31,05 SAY LINHA_11
      @ 32,05 SAY LINHA_12
      @ 33,05 SAY LINHA_13
      @ 34,05 SAY LINHA_14
      @ 35,05 SAY LINHA_15
      @ 36,05 SAY LINHA_16
      @ 37,05 SAY LINHA_17
      @ 38,05 SAY LINHA_18
      @ 39,05 SAY LINHA_19
      IF eREMETENTE
	 PCENTRA("---------------------------------------",43)
		 PCENTRA( sisTIT1,44)
		 PCENTRA( sisTIT1,44)
      ELSE
		 PCENTRA( "---------------------------------------",43)
         IF eNOME_REME <> SPACE(40)
			PCENTRA(eNOME_REME,44)
			PCENTRA(eNOME_REME,44)
         ELSE
            SELE 1
            eTEMPOR = CLIENTE
			PCENTRA(eTEMPOR,44)
			PCENTRA(eTEMPOR,44)
         ENDIF
	  ENDIF
      @ 00,00 SAY " "
      eCONTA = eCONTA + 1
      SELE 1
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   SELE 1
   SKIP
ENDDO
SET DEVICE TO SCREEN
CLOSE DATABASES
RETURN nil
***********************  fim ***********************************
