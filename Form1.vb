Public Class Form1 'Max Airey Y11 Ass1 - Deal or No Deal
    Dim firstCase As Boolean 'to keep track of whether the player has selected their personal case or not (binary)
    Dim availableCases As Integer 'keep track of the number of unopened cases including the personal case for use in starting the bank offer and arithmetic operation in the bank offer sub
    Dim selectedCase As Integer 'is assigned the case # of the case selected that turn
    Dim personalCase As Integer 'stores the case # of the personal case
    Dim currValTotal As Integer 'stores the cumulative values of all the unopened cases for arithmetic operation in the bank offer sub
    Dim bankOfferValue As Single 'the bank offer value after the rounding to 2 decimal places
    Dim chosenFinalCase As Integer 'is assigned the case # of the final selected case at a non-bank ending
    Dim caseValues(12) As Integer 'an array of 13 to store and reference the pre determined case values in (caseValues(0) is ignored and used as a buffer for consistency and to save many lines of dumb code)
    Dim finalCaseDecision As Boolean 'to keep track of whether the player is making their final decision in a non-bank ending (binary)
    Dim bankOfferResult As String 'is assigned the output of the bank offer sub-program
    Dim bankOfferMsg As String 'is assigned the message displayed on the bank offer sub-program. this is stored in a separate variable because of errors occouring due to too many arguments when creating the message box.
    Public Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load 'upon program strtup the following variables are assigned values (because you can't assign values in the form1 root)
        availableCases = 12 'the amount of available cases, to be subtracted from as cases are selected (except personal case selection)
        firstCase = True 'causes the program to recognise that the player is picking their first case
        currValTotal = 9538 '9538 is the cumulative total of the values of the cases, will be subtracted from as cases are removed
        caseValues = {0, 1000, 10, 1, 5, 2000, 5000, 500, 50, 20, 750, 2, 200} 'assigning the values of the array in order of the cases respectively ({buffer}, case1, case 2, {etc.}
    End Sub 'ends pre load of variables
    Public Sub bankOffer() 'bank offer subprogram begins
        bankOfferValue = (currValTotal / availableCases) * 0.85 'divides the current cumulative total of the values of the unopened cases by the amount of unopened cases, this number is then multiplied by 0.85
        bankOfferValue = Math.Round([bankOfferValue], 2) 'rounds the "bankOfferValue" into 2 decimal places, is done on a sepearate line from "(currValTotal / availableCases) * 0.85" due to unknown restrictions on the math.round function
        bankOfferMsg = ("The bank has offered to buy your case for the price: $" & bankOfferValue & ". Will you accept this offer?") 'is made separate from the msgbox due to having many arguments
        bankOfferResult = MsgBox(bankOfferMsg, vbYesNo, "Bank Offer") 'the value of bankOfferResult is determined whether they press yes or no
        If bankOfferResult = vbYes Then 'if they clicked yes:
            MsgBox("Congratulations! " & "You have won a total of $" & bankOfferValue & " , your personal case was worth $" & caseValues([personalCase]) & "! Please play again :)") 'prints message box saying how much they won and how much their personal case was worth
            End 'terminates the program
        Else 'if bankOfferResult equals anything but vbYes:
            MsgBox("Bank's offer declined") 'Prints a confimation dialogue that says " " "
        End If 'no more if statements
        bankOfferHistoryLst.Items.Add("$" & bankOfferValue) 'adds the value of the bank offer to the list box
        If availableCases = 2 Then 'if there is 2 cases remaining including the personal case:
            chooseLbl.Text = "Please choose the case who's value you shall win!" 'changes the label to become "Please choose the case who's value you shall win!"
            personalCaseBtn.Enabled = True 're enables the personal case button to be pressable
            finalCaseDecision = True 'tells the program that it is in the stage of final case selection/decision
        End If 'no mmore if statements
    End Sub 'bank offer subprogram ends
    Public Sub caseChoosing() 'begins case-choosing subprogram
        If firstCase = True Then 'is always true on startup and never again after first case.
            personalCase = selectedCase 'makes personalCase equal to selectedCase
            firstCase = 0 'makes firstCase inequal to True so this 'if statement' will never repeat this section
            chooseLbl.Text = "Please choose the next case to be opened" 'changes the label text from "Please choose your starting case" to "Please choose the next case to be opened"
            personalCaseBtn.Text = ("Case #" & personalCase) 'makes the text of personalCaseBtn equal to the text of the selected button
        Else 'if firstCase isnt true: (if-statement-ception)
            If finalCaseDecision = True Then 'if the player is choosing between the final case and their personal case
                chosenFinalCase = selectedCase 'the won case is the case that they chose (selectedCase)
                MsgBox("Congratulations! " & "You have won a total of $" & caseValues([chosenFinalCase]) & "! Please play again :)") 'its a message box bruh, work it out yourself
                End 'terminates the program after previous message box is closed or dismissed
            Else 'if it isnt the first case selection or the final case decision
                currValTotal = currValTotal - caseValues(selectedCase) 'the current total value of all the cases is deducted by the value of the most recent selected case
                availableCases = availableCases - 1 'the amount of cases remaining (including the already selected personal case) is updated by removing 1 from the value
                Select Case selectedCase 'if they have selected the case:
                    Case 1 'relative to the case number chosen on that turn
                        val1000Lbl.BackColor = Color.Red 'turns the case's value label back color to red to indicate that value is gone
                    Case 2
                        val10Lbl.BackColor = Color.Red
                    Case 3
                        val1Lbl.BackColor = Color.Red
                    Case 4
                        val5Lbl.BackColor = Color.Red
                    Case 5
                        val2000Lbl.BackColor = Color.Red
                    Case 6
                        val5000Lbl.BackColor = Color.Red
                    Case 7
                        val500Lbl.BackColor = Color.Red
                    Case 8
                        val50Lbl.BackColor = Color.Red
                    Case 9
                        val20Lbl.BackColor = Color.Red
                    Case 10
                        val750Lbl.BackColor = Color.Red
                    Case 11
                        val2Lbl.BackColor = Color.Red
                    Case 12
                        val200Lbl.BackColor = Color.Red
                End Select 'no more casewhere
                Select Case availableCases 'if availableCases = 8 or 5 or 4 or 3 or 2: the bank offer sub-program begins
                    Case 8, 5, 4, 3, 2
                        bankOffer()
                End Select 'no more casewhere
            End If 'no more if statement
        End If 'no more if statement
    End Sub 'ends case-choosing subprogram
    Private Sub case1Btn_Click(sender As Object, e As EventArgs) Handles case1Btn.Click 'when button(#) is clicked:
        selectedCase = 1 'tells the program the case number of the selected case
        case1Btn.Enabled = False 'disables the button so it may not be pressed
        caseChoosing() 'opens case choosing sub-program
    End Sub
    Private Sub case2Btn_Click(sender As Object, e As EventArgs) Handles case2Btn.Click
        selectedCase = 2
        case2Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case3Btn_Click(sender As Object, e As EventArgs) Handles case3Btn.Click
        selectedCase = 3
        case3Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case4Btn_Click(sender As Object, e As EventArgs) Handles case4Btn.Click
        selectedCase = 4
        case4Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case5Btn_Click(sender As Object, e As EventArgs) Handles case5Btn.Click
        selectedCase = 5
        case5Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case6Btn_Click(sender As Object, e As EventArgs) Handles case6Btn.Click
        selectedCase = 6
        case6Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case7Btn_Click(sender As Object, e As EventArgs) Handles case7Btn.Click
        selectedCase = 7
        case7Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case8Btn_Click(sender As Object, e As EventArgs) Handles case8Btn.Click
        selectedCase = 8
        case8Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case9Btn_Click(sender As Object, e As EventArgs) Handles case9Btn.Click
        selectedCase = 9
        case9Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case10Btn_Click(sender As Object, e As EventArgs) Handles case10Btn.Click
        selectedCase = 10
        case10Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case11Btn_Click(sender As Object, e As EventArgs) Handles case11Btn.Click
        selectedCase = 11
        case11Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub case12Btn_Click(sender As Object, e As EventArgs) Handles case12Btn.Click
        selectedCase = 12
        case12Btn.Enabled = False
        caseChoosing()
    End Sub
    Private Sub personalCaseBtn_Click(sender As Object, e As EventArgs) Handles personalCaseBtn.Click
        selectedCase = personalCase 'makes the selected case equal to the personal case number
        caseChoosing() 'opens the case choosing sub-program
    End Sub
End Class 'end of program (not termination)