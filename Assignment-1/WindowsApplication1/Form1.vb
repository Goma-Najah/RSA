Imports System.Numerics
Public Class Form1
    'Generater keys
    Dim q As Integer
    Dim p As Integer
    Dim n As Integer
    Dim z As Integer
    Dim ee As System.Numerics.BigInteger = 0
    'Generater keys
    'Decryption
    Dim d2 As System.Numerics.BigInteger = 0
    Dim n2 As Integer
    'Decryption
    'Encryption
    Dim e1 As System.Numerics.BigInteger = 0
    Dim n1 As Integer
    'Encryption
    'Randome d
    Dim dd As System.Numerics.BigInteger = 0
    Dim ddd
    Dim max As Integer
    Dim min As Integer
    'Randome d
    'Ascii to Decimal,Decimal To hex 'Encryption
    Dim Ciphertext As String
    'Ascii to Decimal,Decimal To hex 'Encryption
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If Not IsNumeric(TextBox1.Text) Then
            MsgBox("Enter (p) as a Number", , "Alert")
        Else
            If Not IsNumeric(TextBox2.Text) Then
                MsgBox("Enter (q) as a Number", , "Alert")
            Else
                Dim i, j As Integer
                Dim t As Boolean
                i = TextBox1.Text
                t = True
                For j = 2 To (i - 1)
                    If i Mod j = 0 Then
                        t = False
                        Exit For
                    End If
                Next j
                If t Then
                    Dim ii, jj As Integer
                    Dim tt As Boolean
                    ii = TextBox2.Text
                    tt = True
                    For jj = 2 To (ii - 1)
                        If ii Mod jj = 0 Then
                            tt = False
                            Exit For
                        End If
                    Next jj
                    If tt Then
                        p = TextBox1.Text
                        q = TextBox2.Text
                        n = p * q
                        z = (p - 1) * (q - 1)
                        Label4.Visible = True
                        Label5.Visible = True
                        Label3.Visible = True
                        Label18.Visible = True
                        Label3.Text = n
                        Label18.Text = z
                        TextBox5.Text = n
                        TextBox12.Text = n
                        If n < 127 Then
                            MsgBox("(n) should be greater than 127, Please try again!", , "Error")
                            n = 0
                            z = 0
                            TextBox1.Text = ""
                            TextBox2.Text = ""
                            Label4.Visible = False
                            Label5.Visible = False
                            Label3.Visible = False
                            Label18.Visible = False
                        Else
                            Button2.Enabled = True
                        End If
                    Else
                        MsgBox(ii & "  (q) is not a prime Number", , "Error")
                    End If
                Else
                    MsgBox(i & "  (p) is not a prime Number", , "Error")
                End If
            End If
        End If
    End Sub
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        ee = TextBox3.Text
        If Not IsNumeric(TextBox3.Text) Then
            MsgBox("Enter (e) as a Number", , "Alert")
        Else
            If ee > n Then
                MsgBox("(e) should be smaller than (n) ", , "Alert")
            Else
                Dim j As Integer
                Dim t As Boolean
                t = True
                For j = 2 To (ee - 1)
                    If ee Mod j = 0 Then
                        t = False
                        Exit For
                    End If
                Next j
                If t Then
                    'Randome d
                    Do While ddd <> 1
                        Static staticRandomGenerator As New System.Random
                        max += 1
                        dd = staticRandomGenerator.Next(If(min > max, max, min), If(min > max, min, max))
                        ddd = ee * dd Mod z
                    Loop
                    'Randome d 
                    Label20.Text = n
                    Label21.Text = n
                    Label9.Text = (ee).ToString("N0")
                    Label19.Text = (dd).ToString("N0")
                    TextBox4.Text = (ee).ToString("N0")
                    TextBox13.Text = (dd).ToString("N0")
                Else
                    MsgBox(" (e) is not a prime Number", , "Error")
                End If
            End If
        End If
    End Sub
    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        Application.Exit()
    End Sub
    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        Application.Exit()
    End Sub
    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        Application.Exit()
    End Sub
    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        If Not IsNumeric(TextBox4.Text) Or Not IsNumeric(TextBox5.Text) Or TextBox6.Text = Nothing Then
            MsgBox("Please, Enter public key e,n and plaintext", , "Alert")
        Else
            e1 = TextBox4.Text
            n1 = TextBox5.Text
            'Ascii to Decimal 'Encryption
            Dim plaintext As String
            plaintext = TextBox6.Text
            Ciphertext = plaintext.Select(Function(t) (BigInteger.Pow(Asc(t), e1) Mod n1).ToString()).Aggregate(Function(t1, t2) t1 & "," & t2)
            TextBox7.Text = Ciphertext
            'Ascii to Decimal 'Encryption

            'Decimal To hex
            Dim input = Ciphertext
            Dim CiphertextHex = String.Join("t", input.Split(","c).Select(Function(x) Convert.ToInt32(x).ToString("x")))            'Replace 0 to any char you want and chane x small to get capital chars
            TextBox8.Text = CiphertextHex

            Dim CiphertextHexc = String.Join("t", input.Split(","c).Select(Function(x) Convert.ToInt32(x).ToString("x")))            'Replace 0 to any char you want and chane x small to get capital chars
            TextBox9.Text = CiphertextHexc
            'Decimal To hex
        End If
    End Sub
    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        TextBox9.Text = ""
        TextBox10.Text = ""
        TextBox11.Text = ""
    End Sub
    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        e1 = 0
        n1 = 0
        TextBox6.Text = ""
        TextBox7.Text = ""
        TextBox8.Text = ""
    End Sub
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Button2.Enabled = False
        Label4.Visible = False
        Label5.Visible = False
        TextBox1.Text = ""
        TextBox2.Text = ""
        TextBox3.Text = ""
        Label3.Text = ""
        Label18.Text = ""
        Label9.Text = ""
        Label19.Text = ""
        Label20.Text = ""
        Label21.Text = ""
        p = 0
        q = 0
        n = 0
        z = 0
        ee = 0
        dd = 0
        ddd = 0
    End Sub
    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        If Not IsNumeric(TextBox13.Text) Or Not IsNumeric(TextBox12.Text) Or TextBox9.Text = Nothing Then
            MsgBox("Please, Enter private key d,n and ciphertext(in hex)", , "Alert")
        Else
            d2 = TextBox13.Text
            n2 = TextBox12.Text
            'Hex to Decimal
            Dim CiphertextDec = TextBox9.Text
            Dim CiphertextDec1 = String.Join(",", CiphertextDec.Split("t"c).Select(Function(x) Convert.ToInt32(x, 16).ToString()))
            TextBox10.Text = CiphertextDec1
            'Hex to Decimal

            'Decimal to Ascii 'Decryption
            Dim CiphertexttoAscii As String = TextBox10.Text
            '  TextBox11.Text = ""
            Dim Parts As String() = CiphertexttoAscii.Split(","c)
            For Each Part As String In Parts
                Dim Plaintext1 As BigInteger = (BigInteger.Pow((Part), d2) Mod n2)
                Dim PlaintextAscii As Char = Chr(Plaintext1)
                'Finally
                'TextBox2.Text = TextBox2.Text + " There is no ascii code for: " + Plaintext1
                ' End Try
                'TextBox11.Text = TextBox11.Text + (Plaintext1).ToString("N0") 'To show decimal
                TextBox11.Text = TextBox11.Text + PlaintextAscii 'To show Ascii
            Next
            'Decimal to Ascii 'Decryption
        End If
    End Sub
    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        AboutBox1.Show()
    End Sub
    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles Button11.Click
        If Not IsNumeric(TextBox13.Text) = Nothing Then
            Clipboard.SetText(TextBox13.Text)
        Else
            MsgBox("No key is selected to copy", , "Alert")
        End If
    End Sub
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        If Not IsNumeric(TextBox4.Text) = Nothing Then
            Clipboard.SetText(TextBox4.Text)
        Else
            MsgBox("No key is selected to copy", , "Alert")
        End If
    End Sub
End Class