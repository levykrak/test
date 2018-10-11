'wersja 21.01.2011
$regfile = "m8def.dat"                                      'informuje kompilator o pliku 'dyrektyw mikrokontrolera
$crystal = 1000000                                          'informuje kompilator o czêstotliwoœci oscylatora taktuj¹cego mikrokontroler
'$baud = 1200

Config Portd = Output
Config Pinc.4 = Output
Config Pinc.5 = Output
Config Pinc.0 = Input
Config Pinc.1 = Input
Config Pinc.2 = Input
Config Pinb.0 = Output
Config Pinb.1 = Output
Config Pinb.2 = Output
Config Pinb.3 = Output
Config Pinb.4 = Output
Config Pinb.5 = Output
Config Pinb.6 = Output
Config Timer1 = Timer , Prescale = 1

Declare Sub Pobr_znaku_cyfra(cyfra As Byte)
Declare Sub Pobr_znaku_slupek(cyfra As Byte)
Config Adc = Single , Prescaler = Auto , Reference = Avcc


On Timer1 Mult_wysw

Dim A As Byte , B As Byte , C As Byte , E As Byte , F As Byte , G As Byte
Dim Paliwo As Byte
Dim Temp As Byte , Temp1 As Long , Temp2 As Bit
Dim Volt As Word
Dim Fuel As Word
Dim Temperature As Word , Temp_temperatury As Single , Abc As Byte , Abc_1 As Byte , I_temp As Byte

Dim Nr_wysw As Byte
Dim Wart As Byte

W1 Alias Portb.0
W2 Alias Portb.1
W3 Alias Portb.2
W4 Alias Portb.3
W5 Alias Portb.4
W6 Alias Portb.5
W7 Alias Portb.6

Enable Interrupts
Enable Timer1
Start Adc

Paliwo = 10
A = 8
B = 8
C = 8
E = 8
F = 8
G = 8
Wait 1


Do
Fuel = Getadc(0)
Volt = Getadc(1)
Temperature = Getadc(2)

Wait 1
Stop Timer1                                                 'eliminujemy efekt mrygniecia
'#######Woltomierz  - wyswietlacze E F G
                                            'jednostki z adc1
Temp1 = Volt * 196                                          'przeliczanie dzielnika napiecia i napiecia na jedna jednostke
Temp1 = Temp1 / 1000                                        'uzyskujemy liczbe 3 wartosciowa
Temp1 = Temp1 - 4                                           'uchyb dziesietne czesci

Temp = Temp1 / 100                                          'wyliczanie poszczegolnych cyfr wyswietlacza
E = Temp
Temp = Temp1 Mod 100
Temp = Temp / 10
F = Temp
Temp = Temp1 Mod 10
G = Temp
'#####koniec woltomierza
Start Timer1

'######## paliwo
Fuel = Fuel - 512
Fuel = Fuel / 16
Paliwo = Fuel - 1

If Paliwo = 0 And Temp2 = 0 Then                            'zrobimy se mryganie jesli poziom ponizej 6 Litrow
 Paliwo = 1
 Temp2 = 1
 Elseif Paliwo = 0 And Temp2 = 1 Then
 Paliwo = 0
 Temp2 = 0
End If
'######## koniec obslugi paliwa
Start Timer1


'#########  temperatura ABC#####

If Temperature > 16 And Temperature < 308 Then

'   If I_temp = 5 Then
'   I_temp = 0
   Select Case I_temp
      Case 0:
      Temp_temperatury = 1024 / Temperature
      Case 1:
      Temp_temperatury = Temp_temperatury - 1
      Case 2:
      Temp_temperatury = -216.3759 / Temp_temperatury       'obliczamy sobie temperaturke
      Temp_temperatury = Temp_temperatury + 130.639405
      Case 3:
      Temp_temperatury = Round(temp_temperatury)
      Abc_1 = Temp_temperatury
      Case 4:
      Abc = Abc + Abc_1
      Abc = Abc / 2
      Case 5:
      A = Abc / 100
      B = Abc Mod 100
      B = B / 10                                            'konwertujemy kazda cyfre do wyswietlacza
      C = Abc Mod 10

   End Select

Incr I_temp
If I_temp = 6 Then

I_temp = 0
End If


Elseif Temperature < 17 Then
A = 10
B = 10
C = 10
I_temp = 0
Elseif Temperature > 307 Then
A = 10
B = 10
C = 10
I_temp = 0                                                  ' zerujemy case do obliczania temperatury
End If


'######## koniec przeliczania temperatury######


Loop
End
'######## KONIEC PETLI GLOWNEJ PROGRAMU #################################################


Sub Pobr_znaku_cyfra(cyfra As Byte)                         'wyswietlacze 7-segmentowe

   If Cyfra < 11 Then
      Portd = Lookup(cyfra , Kody7seg)
   Else
      Portd = 255
   End If
End Sub

Sub Pobr_znaku_slupek(paliwo As Byte)                       'sterowanie wyswietlaczem paliwa

   If Cyfra < 11 Then
      Portd = Lookup(paliwo , Kod10seg_portd)
      Portc = Lookup(paliwo , Kod10seg_portc)
   Else
      Portd = 255
      Portc = 255
   End If

End Sub


Mult_wysw:
Load Timer1 , 2000

Set W1
Set W2
Set W3
Set W4
Set W5
Set W6
Set W7
Select Case Nr_wysw

   Case 0:                                                  'wyswietlacz 1
      Call Pobr_znaku_cyfra(a)
      Reset W1
   Case 1:
      Call Pobr_znaku_cyfra(b)
      Reset W2
   Case 2:
      Call Pobr_znaku_cyfra(c)
      Reset W3
   Case 3:                                                  'wyswietlacz paliwa
      Call Pobr_znaku_slupek(paliwo)
      Reset W4
   Case 4:
      Call Pobr_znaku_cyfra(e)
      Reset W5
   Case 5:
      Call Pobr_znaku_cyfra(f)
      Reset W6
   Case 6:
      Call Pobr_znaku_cyfra(g)
      Reset W7
   End Select
   Incr Nr_wysw

   If Nr_wysw = 7 Then
   Nr_wysw = 0
   End If
Return


'       0            1              2        3              4
'     &B DP G F E D C B A   dla zera swieci
Kody7seg:
Data &B01000000 , &B01111001 , &B00100100 , &B00110000 , &B00011001
Data &B00010010 , &B00000010 , &B01111000 , &B00000000 , &B00010000
Data &B10111111                                             '- dla wyswietlacza

Kod10seg_portd:
Data &B11111111 , &B11111110 , &B11111100 , &B11111000 , &B11110000
Data &B11100000 , &B11000000 , &B10000000 , &B00000000 , &B00000000
Data &B00000000

Kod10seg_portc:
Data &B1111111 , &B1111111 , &B1111111 , &B1111111 , &B1111111
Data &B1111111 , &B1111111 , &B1111111 , &B1111111 , &B1101111
Data &B1001111

'Temperatury:
'Data 307% , 302% , 290% , 278% , 265% , 252% , 238% , 224% , 209% , 194% , 178%
'Data 162% , 145% , 127% , 108% , 89% , 69% , 48% , 26% , 17%