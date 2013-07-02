      program test

		use REAL_PRECISION
		real(KIND = R8), dimension(12):: x, lb, ub
		integer i;
	
		open(101, FILE="X0.dat", STATUS='OLD')
		read(101,*)x
		close(101)

		do i=1,12
			lb(i)=0
			ub(i)=1
		end do

      call nomad( 12 , 1 , x , lb , ub , 1000 , 1 )

		open(102, FILE="X0.dat",STATUS='REPLACE')
		write(102,*)x
		close(102)


      end 




      subroutine bb(xx,fx)
				use TestObj_MOD
				use REAL_PRECISION
				implicit none

				real(kind = R8), dimension(12), intent(IN) :: xx
				real(kind = R8), dimension(1), intent(OUT):: fx
	
				call TestObj(xx, fx)
	      	return
		end
