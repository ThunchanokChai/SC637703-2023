#Assignment_CH1
#Q1
#### Input part ####
A=matrix(c(1,-1,4,1,1,3,4,3,2),3,3)
A
B=matrix(c(3,7,2,-2,1,3,4,0,5),3,3)
B
det(A%*%B)
x=matrix(c(1,-1,2),3,1)
x
y=matrix(c(3,2,1),3,1)
y
xT=t(x)
xT
BT=t(B)
BT
### x^T A x
Ans_Q1.1=xT%*%A%*%x
#result
Ans_Q1.1
### x^T y
Ans_Q1.2=xT%*%y
#result
Ans_Q1.2
###  y B^T
#cannot find answer of this question because y is (3,1) ,Bt is (3,3)
###  x^T A y
Ans_Q1.4=xT%*%A%*%y
#result
Ans_Q1.4

#Q2
#### Input part ####
A=matrix(c(5,2,3,4,-3,7,4,1,2),3,3)
A
B=matrix(c(1,0,1,0,1,2,1,0,3),3,3)
B
AB=A%*%B
AB
###Q2.1
####find det(AB)
Ans_det_AB=det(AB)
###result
Ans_det_AB
###the result is 46

###find det(A)det(B)
Ans_det_A_det_B=det(A)*det(B)
###result
Ans_det_A_det_B
###the result is 46
#Therefore det(AB)=det(A)det(B)


### Show that det(AB)=det(A)det(B)
Ans_det_AB==Ans_det_A_det_B


###Q2.2
###Find (AB)T
Ans_ABT=t(AB)
###result
Ans_ABT
###Find B^T A^T
Ans_BT_AT=t(B)%*%t(A)
###result
Ans_BT_AT
### Show that (AB)T=B^T A^T
Ans_ABT==Ans_BT_AT

