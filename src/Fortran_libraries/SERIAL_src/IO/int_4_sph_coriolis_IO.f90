!int_4_sph_coriolis_IO.f90
!      module int_4_sph_coriolis_IO
!
!     Written by H. Matsui on March, 2010
!
!      subroutine write_int_4_sph_coriolis
!      subroutine read_int_4_sph_coriolis
!
      module int_4_sph_coriolis_IO
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_int_4_sph_coriolis
!
      use m_int_4_sph_coriolis_IO
!
      integer(kind = kint) :: j3, j1, j2
!
!
      write(*,'(a,a)') 'Write tri-integration data file: ',             &
     &                trim(sph_cor_file_name)
      open(id_sph_cor,file=sph_cor_file_name)
      write(id_sph_cor,'(a)') '# ----- rotate.dat -----'
      write(id_sph_cor,'(a)') '#  truncation'
      write(id_sph_cor,'(i10)')  ltr_cor_IO
!
      j1 = 2
      write(id_sph_cor,'(a)') '# j1, l2_gl, j3_gl, Ki/pi'
      do j3 = 1 ,jmax_cor_IO
        do j2 = 1, 2
          write(id_sph_cor,'(3i10,1pE25.15e3)') j1,                     &
     &                jgl_kcor_IO(j3,j2,j1), j3, gk_cor_IO(j3,j2,j1)
        end do
      end do
      write(id_sph_cor,'(a)') '# j1, l2_gl, j3_gl, Li/pi'
      do j3 = 1 ,jmax_cor_IO
        write(id_sph_cor,'(3i10,1pE25.15e3)') j1,                       &
     &                jgl_lcor_IO(j3,1,j1), j3, el_cor_IO(j3,1,j1)
      end do
!*
!
      do j1 = 1, 3, 2
        write(id_sph_cor,'(a)') '# j1, l2_gl, j3_gl, Ki/pi'
        do j3 = 1 ,jmax_cor_IO
          do j2 = 1, 4
            write(id_sph_cor,'(3i10,1pE25.15e3)') j1,                   &
     &                jgl_kcor_IO(j3,j2,j1), j3, gk_cor_IO(j3,j2,j1)
          end do
        end do
!*
        write(id_sph_cor,'(a)') '# j1, l2_gl, j3_gl, Li/pi'
        do j3 = 1, jmax_cor_IO
          do j2 = 1, 2
            write(id_sph_cor,'(3i10,1pE25.15e3)') j1,                   &
     &                jgl_lcor_IO(j3,j2,j1), j3, el_cor_IO(j3,j2,j1)
          end do
        end do
      end do
      close(id_sph_cor)
!
      call deallocate_int_sph_cor_IO
!
      end subroutine write_int_4_sph_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_int_4_sph_coriolis
!
      use m_int_4_sph_coriolis_IO
      use skip_comment_f
!
      integer(kind = kint) :: j3, j1, j2, itmp
      character(len=255) :: character_4_read
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     & 'Read integrals for coriolis: ', trim(sph_cor_file_name)
      open(id_sph_cor,file=sph_cor_file_name)
!
      call skip_comment(character_4_read,id_sph_cor)
      read(character_4_read,*) ltr_cor_IO
!
!
      call allocate_int_sph_cor_IO
!
      j1 = 2
      call skip_comment(character_4_read,id_sph_cor)
      read(character_4_read,*) itmp, jgl_kcor_IO(1,1,j1), itmp,         &
     &                gk_cor_IO(1,1,j1)
      read(id_sph_cor,*) itmp, jgl_kcor_IO(1,2,j1), itmp,               &
     &                gk_cor_IO(1,2,j1)
      do j3 = 2 ,jmax_cor_IO
        do j2 = 1, 2
          read(id_sph_cor,*) itmp, jgl_kcor_IO(j3,j2,j1), itmp,         &
     &                gk_cor_IO(j3,j2,j1)
        end do
      end do
!
      call skip_comment(character_4_read,id_sph_cor)
      read(character_4_read,*) itmp, jgl_lcor_IO(1,1,j1), itmp,         &
     &                el_cor_IO(1,1,j1)
      do j3 = 2 ,jmax_cor_IO
        read(id_sph_cor,*) itmp, jgl_lcor_IO(j3,1,j1), itmp,            &
     &                el_cor_IO(j3,1,j1)
      end do
!*
!
      do j1 = 1, 3, 2
        call skip_comment(character_4_read,id_sph_cor)
        read(character_4_read,*) itmp, jgl_kcor_IO(1,1,j1), itmp,       &
     &                          gk_cor_IO(1,1,j1)
        do j2 = 2, 4
          read(id_sph_cor,*) itmp, jgl_kcor_IO(1,j2,j1), itmp,          &
     &                gk_cor_IO(1,j2,j1)
        end do
        do j3 = 2, jmax_cor_IO
          do j2 = 1, 4
            read(id_sph_cor,*) itmp, jgl_kcor_IO(j3,j2,j1), itmp,       &
     &                gk_cor_IO(j3,j2,j1)
          end do
        end do
!*
        call skip_comment(character_4_read,id_sph_cor)
        read(character_4_read,*) itmp, jgl_lcor_IO(1,1,j1), itmp,       &
     &                el_cor_IO(1,1,j1)
        read(id_sph_cor,*) itmp, jgl_lcor_IO(1,2,j1), itmp,             &
     &                el_cor_IO(1,2,j1)
        do j3 = 2, jmax_cor_IO
          do j2 = 1, 2
            read(id_sph_cor,*) itmp, jgl_lcor_IO(j3,j2,j1), itmp,       &
     &                el_cor_IO(j3,j2,j1)
          end do
        end do
      end do
      close(id_sph_cor)
!
      end subroutine read_int_4_sph_coriolis
!
! -----------------------------------------------------------------------
!
      end module int_4_sph_coriolis_IO
