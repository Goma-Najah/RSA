Public Class Form1
    Dim q
    Dim p
    Dim n
    Dim z
    Dim r
    Dim ee
    Dim dd
    Dim ddd
    Dim max As Integer
    Dim min As Integer
    Dim c As String
    Dim m As Double
    Dim m1 As String



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
                        Label3.Text = n
                        Label18.Visible = True
                        Label18.Text = z
                        TextBox5.Text = n
                        TextBox12.Text = n

                        ' If n < 127 Then
                        '  MsgBox(n & "  < 127 ", , "Error")
                        'Else
                        Button2.Enabled = True

                        ' End If


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
                MsgBox(" (e) must be smaller than (n) ", , "Alert")
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

                    Do While ddd <> 1


                        Static staticRandomGenerator As New System.Random
                        max += 1
                        dd = staticRandomGenerator.Next(If(min > max, max, min), If(min > max, min, max))


                        ddd = ee * dd Mod z

                    Loop

                    Label20.Text = n
                    Label21.Text = n
                    Label9.Text = ee
                    Label19.Text = dd
                    TextBox4.Text = ee
                    TextBox13.Text = dd
                Else
                    MsgBox(ee & " (e) is not a prime Number", , "Error")
                End If

            End If
        End If

    End Sub

    Private Sub TabPage2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabPage2.Click
        Label4.Text = ee
        Label5.Text = n
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
        Dim sText As String
        Dim i As Integer
        Dim lenText As Integer
        Dim sASC As String

        sText = TextBox6.Text
        lenText = Len(sText)
        For i = 1 To lenText
            sASC = sASC & CStr(Asc(Mid$(sText, i, 1)))
        Next i
        m = sASC
        TextBox7.Text = m
        c = m ^ ee Mod n

        m1 = c ^ dd Mod n
        TextBox8.Text = c
        TextBox9.Text = c

    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        TextBox9.Text = ""
        TextBox10.Text = ""
        TextBox11.Text = ""

    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        TextBox6.Text = ""
        TextBox7.Text = ""
        TextBox8.Text = ""
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Button2.Enabled = False
        TextBox1.Text = ""
        TextBox2.Text = ""
        TextBox3.Text = ""
        Label3.Text = ""
        Label18.Text = ""
        Label9.Text = ""
        Label19.Text = ""
        Label20.Text = ""
        Label21.Text = ""
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click

       
        m1 = 571 ^ 815 Mod 1073
        TextBox10.Text = m1




    End Sub
End Class
