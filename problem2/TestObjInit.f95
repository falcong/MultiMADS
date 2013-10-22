		MODULE TestObjInit_MOD
		USE REAL_PRECISION
		CONTAINS

		SUBROUTINE TestObjInit(X, OBJ)
		IMPLICIT NONE
		REAL(KIND = R8), DIMENSION(2), INTENT(IN):: X
		REAL(KIND = R8), DIMENSION(1), INTENT(OUT):: OBJ

		!local variables
		INTEGER::i, j, k, ii, N, iflag
		REAL(KIND = R8):: f1
		REAL(KIND = R8):: f2
		REAL(KIND = R8):: f3
		REAL(KIND = R8):: g, r
		REAL(KIND = R8)::SumXi, pi
		REAL(KIND = R8), DIMENSION(2):: lb, ub
		REAL(KIND = R8), DIMENSION(3,3):: Ip
		REAL(KIND = R8), DIMENSION(3):: F
		REAL(KIND = R8), DIMENSION(5):: delta

		N = 2
		pi = 3.14159265359

      lb(1:2) = 0.0
		ub(1:2) = 1.0

		f1=0
		f2=0
		f3=0
		iflag=0
		do k=1,N
			if (X(k)<0.0.or.X(k)>1.0) then
!				write(*,*)'infeasible ',X
				OBJ=1.0e13
				iflag=1
				exit
			end if
		end do
		
		if(iflag<1) then
				!write(*,*) 'Sum ',SumXi
				g   = 1 + 10*X(2)
				!write(*,*) 'g ',g
				r = 1 - (X(1)/g)**2 - (X(1)/g)*Sin(8*pi*X(2))

				f1 = X(1)
			  	f2  = g*r
				if(X(1)<=0.5) then
				  	f3  = 0
				else
					f3=1
				end if
				F(1)=f1
				F(2)=f2
				F(3)=f3
				OBJ=f1+f2+f3

		OPEN(85, FILE="RSMInTest.dat", STATUS='OLD', POSITION='APPEND')
		WRITE(85,*)X,f1,f2,f3
		CLOSE(85)

		end if

		END SUBROUTINE TestObjInit

		END MODULE TestObjInit_MOD

