*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXCLUI.PRG
* Data        : 01/01/99
* Objetivo    : EXCLUSAO DE CLIENTES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXCLUI()
SAVE SCREEN TO TELA03
PTIT("EXCLUSAO DE CLIENTES")
SELE 1
USE CONTATO INDEX CONTATO,NUM_CLIE
GO TOP
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   ePESQUISA = SPACE(49)
   @ 05,02 SAY "Cliente     :"  GET ePESQUISA PICT "@! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   READ
   ePESQUISA = ALLTRIM(ePESQUISA)
   SEEK ePESQUISA
   eNUM_CLIE = NUM_CLIE
   DECLARE CAMPO[1],PIC[1],CAB[1],LENS[1]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@S50"
   CAB[1]="  NOME"
   DBEDIT(5,15,5,65,CAMPO,"KEYS_4",PIC,CAB," "," "," ")
   IF F_BLOQR()
      @ 07,02 SAY "CPF ou CGC    : " GET CPF_CGC
      @ 08,02 SAY "RG            : " GET RG
      @ 08,44 SAY "UF : "            GET UF_RG PICT "@!XX"
      @ 09,02 SAY "Emiss∆o       : " GET EMISSAO_RG
      @ 09,41 SAY "Org∆o : "         GET ORGAO_RG
      @ 10,02 SAY "Sexo (M/F/O)  : " GET SEXO PICT "X" VALID SEXO $("MFO")
      @ 10,36 SAY "Nascimento : "    GET NASCIMENTO
      @ 15,02 SAY "Num.Cliente   : " GET NUM_CLIE PICT "@R 9999999999"
      CLEAR GETS
      eNUM_CLIE = NUM_CLIE
      UNLOCK
   ENDIF
   SELE 1
   PLQUA("01")
   PPERG("Confirma (S/N) ? ")
   IF sisRESP = "S"
      IF CLIENTE <> "       "
	 PLQUA("01")
	 PMSGR("Atená∆o !",3)
         PMENSAG("Aguarde, Verificando se o Cliente Possui Unidades ...")
	 SELE 2
	 USE UNI_IMOB INDEX UNI_IMOB
	 GOTO TOP
	 LOCATE FOR PROPRIET_1 = eNUM_CLIE .OR. PROPRIET_2 = eNUM_CLIE
	 IF .NOT. EOF()
	 PLQUA("01")
	 PMSGR("Atená∆o !",3)
         PMENSAG("Cliente em quest∆o Ç Proprietario de Unidades, Exclus∆o Cancelada.")
	    PPARA()
	    F_LACESSO(sisUSUARIO,"CADASTRO CLIENTES","TENTATIVA DE EXCLUSAO CLIENTE No. "+ALLTRIM(STR(eNUM_CLIE,10,0))) 
	 ELSE
	    SELE 1
	    IF F_BLOQR()
	       DELETE
	       UNLOCK
	    ENDIF
	    F_LACESSO(sisUSUARIO,"CADASTRO CLIENTES","EXCLUSAO CLIENTE No. "+ALLTRIM(STR(eNUM_CLIE,10,0))) 
   	    PLQUA("01")
	    PMSGR("Exclus∆o Conclu°da !",3)
	 ENDIF
      ENDIF
      EXIT
   ELSE
      EXIT
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
