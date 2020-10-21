*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_FORN.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE FORNECEDORES DO CONTAS A PAGAR   
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_FORN()
SAVE SCREEN TO TELA07
PTIT( "RELATORIO CADASTRO DE FORNECEDORES")
SELE 2
USE CLAS_FOR INDEX CLAS_FOR
GOTO TOP
PLQUA("01")
PPERG ( "Deseja o Relat‡rio em Ordem Alfab‚tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE FORNECED INDEX FORNECED
   GOTO TOP
ELSE
   SELE 1
   USE FORNECED
   INDEX ON NOME TO TEMP_FOR
   GOTO TOP
ENDIF
PLQUA("01")
eCLASSE = 0
@ 23,00 SAY "Informe a Classe do Fornecedor ou (0) para Todas ? " GET eCLASSE PICT "@R 999"
READ
IF eCLASSE <> 0
   SELE 1
   SET FILTER TO CLASSE = eCLASSE
   GOTO TOP
ENDIF
PLQUA("01")
PPERG ( "Deseja Todas as Informa‡äes do Fornecedor ? ")
IF sisRESP = "S"
   eFAZ = .T.
ELSE
   eFAZ = .F.
ENDIF
SELE 1
GOTO TOP
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"FORNECEDORES.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "FORNECEDORES.TXT"+SPACE(10) 
ENDIF     
PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
PPARA()
DO PIMPR
SET DEVICE TO PRINT
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,120 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,121 SAY "Relat: SAN007"
   @ 03,40  SAY sisLETRAE+"CADASTRO DE FORNECEDORES DO CONTAS A PAGAR"+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   IF eFAZ
      @ 06,02  SAY "NOME"
      @ 06,52  SAY "CPF/CGC"
      @ 06,70  SAY "RG"
      @ 06,88  SAY "UF RG"
      @ 06,96  SAY "EMISSAO RG"
      @ 06,107 SAY "ORGAO RG"
      @ 06,117 SAY "NASCIMENTO"
      @ 06,129 SAY "SEXO"
      @ 07,02  SAY "ENDERECO"
      @ 07,44  SAY "BAIRRO"
      @ 07,74  SAY "CIDADE"
      @ 07,114  SAY "ESTADO"
      @ 07,34  SAY "CEP"
      @ 07,44  SAY "TEL 1"
      @ 07,60  SAY "TEL 2"
      @ 08,00 SAY REPLICATE("=",135)
      LIN = 9
   ELSE
      @ 06,02  SAY "CODIGO"
      @ 06,14  SAY "CLIENTE"
      @ 06,64  SAY "ENDERECO"
      @ 06,114 SAY "TELEFONE"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
   ENDIF
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eFAZ
         @ LIN,01  SAY ALLTRIM(NOME)
         @ LIN,52  SAY CPF_CGC
         @ LIN,70  SAY RG
         @ LIN,88  SAY UF_RG
         @ LIN,96  SAY EMISSAO_RG
         @ LIN,107 SAY ORGAO_RG
         @ LIN,117 SAY NASCIMENTO
         @ LIN,130 SAY SEXO
         LIN = LIN + 1
         @ LIN,01  SAY END
         @ LIN,44  SAY BAIRRO
         @ LIN,74  SAY CIDADE
         @ LIN,114 SAY ESTADO
         LIN = LIN + 1
         @ LIN,34  SAY CEP PICT "@R 99999-999"
         @ LIN,44  SAY TEL_1 PICT "@R (XX) XXXXX-XXXXX"
         @ LIN,60  SAY TEL_2 PICT "@R (XX) XXXXX-XXXXX"
         LIN = LIN + 1
         @ LIN,00 SAY REPLICATE("-",135)
         LIN = LIN + 1
      ELSE
         eCLAS_FOR = CLASSE
         SELE 2
         SEEK eCLAS_FOR
         @ LIN,02  SAY DESCRICAO
         @ LIN,02  SAY DESCRICAO
         LIN = LIN + 1
         SELE 1
         @ LIN,02  SAY CODIGO     PICT "@R 99999"
         @ LIN,14  SAY SUBSTR(NOME,1,45)
         @ LIN,64  SAY SUBSTR(END,1,48)
         @ LIN,114 SAY TEL_1 PICT "@R (XX) XXXXX-XXXXX"
         LIN = LIN + 1
      ENDIF
      SKIP
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   @ 58,00 SAY REPLICATE("=",135)
   @ 59,104 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA07
RETURN nil
*************************  fim  *********************************
