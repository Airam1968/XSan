*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_PAGO.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE PAGAMENTOS DE PARCELAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
function con_pago()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE PAGAMENTO DE PARCELAS")
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
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 8
* Cadastro de Parcelas
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 07,02 SAY "Periodo : " GET eDATA_INI
   @ 07,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv lidas !",3)
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
eORIGEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Origem dos Pagamentos (C)arteira,(B)anco ou (A)mbas ? " GET eORIGEM PICT "@!X" VALID eORIGEM $("CBA")
READ
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Tipo de Parcelas (S)inal,(N)ormais,(I)ntermediarias ou (A)mbas ? " GET eTIPO PICT "@!X" VALID eTIPO $("SNIA")
READ
ePROTESTO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Selecionar Títulos em (P)rotesto,(N)ão Protesto ou (A)mbos ? " GET ePROTESTO PICT "@!X" VALID ePROTESTO $("PNA")
READ
PLQUA("01")
eORDEM = SPACE(1)
@ 23,00 SAY "Emissão de Relatório por (O)rdem de Pagamento ou (I)tem (O/I) ? " GET eORDEM PICT "@!X" VALID eORDEM $("OI")
READ
PLQUA("01")
ePERIODO = SPACE(1)
@ 23,00 SAY "Selecionar Pagamentos (A)ntecipado, (P)ostergado, (M)es ou (T)odos (A/P/M/T) ? " GET ePERIODO PICT "@!X" VALID ePERIODO $("APMT")
READ
IF eTIPO_PRO = 1
   SELE 3
   SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5)
   GOTO TOP
ELSE
   SELE 4
   SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5)
   GOTO TOP
ENDIF
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ou (0) para Todas ? " GET eCONTA PICT "@R 999"
   READ
   SELE 7
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA = 0
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Atenção !",3)
         PMENSAG("Conta Não Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eSUB_TOT  = 0
eTOT_TOT  = 0
eTOTAL    = 0
SELE 9
SET EXCLUSIVE ON
USE TEMP_PAR
IF F_BLOQA()
   ZAP
   GOTO TOP
   UNLOCK
ENDIF
SET EXCLUSIVE OFF
IF eTIPO_PRO = 1
   SELE 3
ELSE
   SELE 4
ENDIF
DO WHILE .NOT. EOF()
   IF eTIPO_PRO = 1
      SELE 3
      eQUADRA = NUM_QUADRA
   ELSE
      SELE 4
      eQUADRA = NUM_BLOCO
   ENDIF
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   SELE 9
   USE TEMP_PAR
   IF F_BLOQA()

	  SEL_PAGA()

	  SEL_DESI()		

      GOTO TOP
      UNLOCK
   ENDIF
   IF eTIPO_PRO = 1
      SELE 3 
      SKIP
   ELSE
      SELE 4
      SKIP
   ENDIF
ENDDO
SELE 9
USE TEMP_PAR
GOTO TOP
IF eORDEM = "D"
   INDEX ON DATA_VENC TO TEMP_PAR
   GOTO TOP
ELSE
   INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
   GOTO TOP
ENDIF
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Parcelas Pagas Neste Periodo !")
   PPARA()
ELSE
   DECLARE CAMPO[16],PIC[16],CAB[16],LENS[16]
   AFIELDS(CAMPO,.T.,LENS)
   CAMPO[1] ="NUM_IMOVEL"
   CAMPO[2] ="NUM_PARC"
   CAMPO[3] ="TIPO_PARC"
   CAMPO[4] ="DATA_VENC"
   CAMPO[5] ="VLR_PARC"
   CAMPO[6] ="DATA_PGTO"
   CAMPO[7] ="VLR_PGTO"
   CAMPO[8] ="COD_BANCO"
   CAMPO[9] ="ORIGEM"
   CAMPO[10]="PROTESTO"
   CAMPO[11]="CONTABIL"
   CAMPO[12]="OBS"
   CAMPO[13]="MULTA"
   CAMPO[14]="COMISSAO"
   CAMPO[15]="PERCENTUAL" 
   CAMPO[16]="CONTA"
   LENS[1] =5
   LENS[2] =5
   LENS[3] =3
   LENS[4] =8
   LENS[5] =19
   LENS[6] =8
   LENS[7] =19
   LENS[8] =12
   LENS[9] =1
   LENS[10]=1
   LENS[11]=1
   LENS[12]=10
   LENS[13]=10
   LENS[14]=10
   LENS[15]=10
   LENS[16]=3
   PIC[1] ="@R99999"
   PIC[2] ="@R99999"
   PIC[3] ="XXX"
   PIC[4] ="99/99/99"
   PIC[5] ="@R 99,999,999,999.99"
   PIC[6] ="99/99/99"
   PIC[7] ="@R 99,999,999,999.99"
   PIC[8] ="@R 999999999999"
   PIC[9] ="@!X"
   PIC[10]="L"
   PIC[11]="L"
   PIC[12]="XXXXXXXXXX"
   PIC[13]="@R 999.99"
   PIC[14]="@R 999.99"
   PIC[15]="@R 999.99"
   PIC[16]="@R 999"
   CAB[1] ="Unidade"
   CAB[2] ="Parcela"
   CAB[3] ="Tipo(N/I)"
   CAB[4] ="Venc."
   CAB[5] ="Parcela"
   CAB[6] ="Pago"
   CAB[7] ="Vlr. Pagamento"
   CAB[8] ="Cod. Banco"
   CAB[9] ="Origem"
   CAB[10]="Protesto"
   CAB[11]="Contabil"
   CAB[12]="Obs."
   CAB[13]="Multa"
   CAB[14]="Tx. perm."
   CAB[15]="Tx. Comis."
   CAB[16]="Cod.Conta"
   @ 10,00 TO 21,79 DOUBLE
   DBEDIT(11,01,21,78,CAMPO,"F_CONS",PIC,CAB,CHR(196),CHR(179),CHR(205))
   PLQUA("01")
   PPERG ( "Deseja Totalização (S/N) ? ")
   IF sisRESP = "S"
      PLIMPA()
	  PCENTRA( "TOTALIZA‡Æo DE PAGAMENTOS",9)
      eTOT_PARC = 0
      eTOT_PAGO = 0
      eTOT_TIT  = 0
      eMULTA    = 0
      GOTO TOP
      PLQUA("01")
	  PMENSAG("Aguarde, Efetuando o processo de Totalização ...")
      DO WHILE .NOT. EOF()
         eTOT_PAGO = eTOT_PAGO + VLR_PGTO
         eTOT_PARC = eTOT_PARC + VLR_PARC
         eTOT_TIT  = eTOT_TIT + 1
         SKIP
      ENDDO
      @ 10,10 SAY "Total Parcelas  : " GET eTOT_PARC PICT "@R 999,999,999,999.99"
      @ 12,10 SAY "Total Pago      : " GET eTOT_PAGO PICT "@R 999,999,999,999.99"
      eMULTA = eTOT_PAGO - eTOT_PARC
      @ 14,10 SAY "Total Multa/Juro: " GET eMULTA    PICT "@R 999,999,999,999.99"
      @ 16,10 SAY "Número Parcelas : " GET eTOT_TIT  PICT "@R 999,999"
      CLEAR GETS
      PPARA()
   ENDIF
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil

*************************  fim  *********************************

