*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CLIE.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE CLIENTES 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CLIE()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE CLIENTES CADASTRADOS")
SELE 1
USE CONTATO INDEX CONTATO,NUM_CLIE
GO TOP
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   ePESQUISA = SPACE(49)
   @ 05,02 SAY "Nome :" GET ePESQUISA
   READ
   ePESQUISA = ALLTRIM(ePESQUISA)
   SEEK ePESQUISA
   DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
   AFIELDS(CAMPO,.T.,LENS)
   CAMPO[2]="CPF_CGC"
   LENS[2]=14
   PIC[1]="@S50"
   PIC[2]="@S14"
   CAB[1]="  NOME"
   CAB[2]="CGC/CPF"
   DBEDIT(5,05,5,79,CAMPO,"KEYS_4",PIC,CAB," "," "," ")
   IF F_BLOQR()
      @ 07,02 SAY "CPF/CGC       : " GET CPF_CGC
      @ 08,02 SAY "RG            : " GET RG
      @ 08,44 SAY "UF : "            GET UF_RG PICT "@!XX"
      @ 09,02 SAY "Emissáo       : " GET EMISSAO_RG
      @ 09,41 SAY "Orgáo : "         GET ORGAO_RG
      @ 10,02 SAY "Sexo (M/F/O)  : " GET SEXO PICT "X" VALID SEXO $("MFO")
      @ 10,36 SAY "Nascimento : "    GET NASCIMENTO
      SET COLOR TO W/N+
      @ 12,02 SAY "ENDERECO RESIDENCIAL"
      SET COLOR TO W/N,+W/R
      * TERCEIRA PAGINA
      @ 14,02 SAY 'Endereáo      : ' GET END_R
      @ 15,02 SAY 'Bairro        : ' GET BAIRRO_R
      @ 16,02 SAY 'Cidade        : ' GET CIDADE_R
      @ 17,02 SAY 'Estado        : ' GET ESTADO_R PICT '@!XX'
      @ 18,02 SAY 'CEP           : ' GET CEP_R   PICT '@R 99999-999'
      @ 19,02 SAY 'Telefone  # 1 : ' GET TEL_R_1 PICT '@R (XX) XXXXX-XXXXX'
      @ 20,02 SAY 'Telefone  # 2 : ' GET TEL_R_2 PICT '@R (XX) XXXXX-XXXXX'
      @ 21,02 SAY 'e-mail        : ' GET MAIL_PROP
      PMENSAG("Pressione Qualquer Tecla Para Continuar ...")
      PPARA()
      PLQUA("01")
      @ 07,01 CLEAR TO 21,79
      SET COLOR TO W/N+
      @ 07,02 SAY "ENDERECO COMERCIAL"
      SET COLOR TO W/N,+W/R
      * QUARTA PAGINA
      @ 09,02 SAY "Empresa       : " GET EMPRESA
      @ 10,02 SAY "Profissao     : " GET PROFISSAO
      @ 11,02 SAY "Endereáo      : " GET END_C
      @ 12,02 SAY "Bairro        : " GET BAIRRO_C
      @ 13,02 SAY "Cidade        : " GET CIDADE_C
      @ 14,02 SAY "Estado        : " GET ESTADO_C PICT "@!XX"
      @ 15,02 SAY "CEP           : " GET CEP_C   PICT "@R 99999-999"
      @ 16,02 SAY "Telefone  # 1 : " GET TEL_C_1 PICT "@R (XX) XXXXX-XXXX X - (XXXXXXXXXXXXXXXXXXXX)"
      @ 17,02 SAY "Telefone  # 2 : " GET TEL_C_2 PICT "@R (XX) XXXXX-XXXX X - (XXXXXXXXXXXXXXXXXXXX)"
      @ 18,02 SAY 'E-Mail        : ' GET MAIL_C          
      @ 20,02 SAY "Correspondencia na Residencia ou Escritorio (R/E) ? " GET OPCAO PICT "@!X" VALID OPCAO $("RE")
      PMENSAG("Pressione Qualquer Tecla Para Continuar ...")
      PPARA()
      @ 07,01 CLEAR TO 21,79
      SET COLOR TO W/N+
      @ 07,02 SAY "OBSERVACOES GERAIS"
      SET COLOR TO W/N,+W/R
      * QUINTA PAGINA
      @ 09,00 GET OBSERVA_1
      @ 10,00 GET OBSERVA_2
      @ 11,00 GET OBSERVA_3
      @ 12,00 GET OBSERVA_4
      @ 13,00 GET OBSERVA_5
      @ 15,00 SAY "Num.Cliente   : " GET NUM_CLIE PICT "@R 9999999999"
      CLEAR GETS
      UNLOCK
   ENDIF
   SELE 1
   PMENSAG("Pressione Qualquer Tecla Para Finalizar ...")
   PPARA()
   PLQUA("01")
   PPERG ( "Continua (S/N) ? ")
   IF sisRESP = "S"
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
