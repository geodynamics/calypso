!sph_global_1d_idx_IO.f90
!      module sph_global_1d_idx_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_rtp_gl_1d_table(id_file)
!      subroutine read_rj_gl_1d_table(id_file)
!
!      subroutine write_rtp_gl_1d_table(id_file)
!      subroutine write_rj_gl_1d_table(id_file)
!
      module sph_global_1d_idx_IO
!
      use m_precision
!
      use m_node_id_spherical_IO
!
      implicit none
!
      character(len=255), private :: character_4_read = ''
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rtp_gl_1d_table(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      ndir_sph_IO = 3
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 1
      ncomp_itbl_1d_IO(3) = 2
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*)                                          &
     &           nidx_sph_IO(1), ist_sph_IO(1), ied_sph_IO(1)
      call allocate_idx_sph_1d1_IO
!
      do i = 1, nidx_sph_IO(1)
        read(id_file,*) idx_gl_1_IO(i), r_gl_1_IO(i)
      end do
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*)                                          &
     &           nidx_sph_IO(2), ist_sph_IO(2), ied_sph_IO(2)
      call allocate_idx_sph_1d2_IO
!
      do i = 1, nidx_sph_IO(2)
        read(id_file,*) idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2))
      end do
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*)                                          &
     &           nidx_sph_IO(3), ist_sph_IO(3), ied_sph_IO(3)
      call allocate_idx_sph_1d3_IO
!
      do i = 1, nidx_sph_IO(3)
        read(id_file,*) idx_gl_3_IO(i,1:ncomp_itbl_1d_IO(3))
      end do
!
      end subroutine read_rtp_gl_1d_table
!
! -----------------------------------------------------------------------
!
      subroutine read_rj_gl_1d_table(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      ndir_sph_IO = 2
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 3
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*)                                          &
     &           nidx_sph_IO(1), ist_sph_IO(1), ied_sph_IO(1)
      call allocate_idx_sph_1d1_IO
!
      do i = 1, nidx_sph_IO(1)
        read(id_file,*) idx_gl_1_IO(i), r_gl_1_IO(i)
      end do
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*)                                          &
     &           nidx_sph_IO(2), ist_sph_IO(2), ied_sph_IO(2)
      call allocate_idx_sph_1d2_IO
!
      do i = 1, nidx_sph_IO(2)
        read(id_file,*) idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2))
      end do
!
      end subroutine read_rj_gl_1d_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rtp_gl_1d_table(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! num. start and end global grids'
      write(id_file,'(a)') '! r-direction'
      write(id_file,'(a)') '!'
      write(id_file,'(3i16)') nidx_sph_IO(1),                           &
     &                        ist_sph_IO(1), ied_sph_IO(1)
      do i = 1, nidx_sph_IO(1)
        write(id_file,'(i16,1pE25.15e3)') idx_gl_1_IO(i), r_gl_1_IO(i)
      end do
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! num. start and end global grids'
      write(id_file,'(a)') '! theta direction'
      write(id_file,'(a)') '!'
      write(id_file,'(3i16)') nidx_sph_IO(2),                           &
     &                        ist_sph_IO(2), ied_sph_IO(2)
      do i = 1, nidx_sph_IO(2)
        write(id_file,'(8i16)') idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2))
      end do
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)')                                              &
     &      '! num. of start and end global grids and modes'
      write(id_file,'(a)') '! phi direction'
      write(id_file,'(a)') '!'
      write(id_file,'(3i16)') nidx_sph_IO(3),                           &
     &                        ist_sph_IO(3), ied_sph_IO(3)
      do i = 1, nidx_sph_IO(3)
        write(id_file,'(8i16)') idx_gl_3_IO(i,1:ncomp_itbl_1d_IO(3))
      end do
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine write_rtp_gl_1d_table
!
! ----------------------------------------------------------------------
!
      subroutine write_rj_gl_1d_table(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! num. start and end global grids'
      write(id_file,'(a)') '! r-direction'
      write(id_file,'(a)') '!'
      write(id_file,'(3i16)') nidx_sph_IO(1),                           &
     &                        ist_sph_IO(1), ied_sph_IO(1)
      do i = 1, nidx_sph_IO(1)
        write(id_file,'(i16,1pE25.15e3)') idx_gl_1_IO(i), r_gl_1_IO(i)
      end do
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! num. start and end global modes'
      write(id_file,'(a)') '! on sphere surface wuth degree and order'
      write(id_file,'(a)') '!'
      write(id_file,'(3i16)') nidx_sph_IO(2),                           &
     &                        ist_sph_IO(2), ied_sph_IO(2)
      do i = 1, nidx_sph_IO(2)
        write(id_file,'(8i16)') idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2))
      end do
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine write_rj_gl_1d_table
!
! ----------------------------------------------------------------------
!
      end module sph_global_1d_idx_IO
