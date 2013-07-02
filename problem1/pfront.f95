  program pfront
		use REAL_PRECISION
		real(KIND = R8), dimension(15):: xfx
		integer ioerr

		open(103, FILE="skyline.dat",STATUS='OLD')
		open(104, FILE="front.dat",STATUS='OLD', POSITION='APPEND')
		do
			read(103,*,IOSTAT=ierr)xfx

			if(ierr > 0) then
				write(*,*) 'Read failed'
				exit
			else if (ierr < 0) then
					exit
			else 
				write(104,*)xfx
			end if
		end do
		close(103)
		close(104)
	end
