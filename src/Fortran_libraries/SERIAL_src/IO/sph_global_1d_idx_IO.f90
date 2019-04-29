!>@file  sph_global_1d_idx_IO.f90
!!       module sph_global_1d_idx_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief BAse routines for spectr indexing data IO
!!
!!@verbatim
!!      subroutine read_rtp_gl_1d_table(id_file, sph_IO)
!!      subroutine read_rj_gl_1d_table(id_file, sph_IO)
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine write_rtp_gl_1d_table(id_file, sph_IO)
!!      subroutine write_rj_gl_1d_table(id_file, sph_IO)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module sph_global_1d_idx_IO
!
      use m_precision
!
      use t_node_id_spherical_IO
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
      subroutine read_rtp_gl_1d_table(id_file, sph_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
      sph_IO%numdir_sph = 3
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 1
      sph_IO%ncomp_table_1d(3) = 2
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%nidx_sph(1),                      &
     &                         sph_IO%ist_sph(1), sph_IO%ied_sph(1)
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(1)
        read(id_file,*) sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i)
      end do
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%nidx_sph(2),                      &
     &                         sph_IO%ist_sph(2), sph_IO%ied_sph(2)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(2)
        read(id_file,*) sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2))
      end do
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%nidx_sph(3),                      &
     &                         sph_IO%ist_sph(3), sph_IO%ied_sph(3)
      call alloc_idx_sph_1d3_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(3)
        read(id_file,*) sph_IO%idx_gl_3(i,1:sph_IO%ncomp_table_1d(3))
      end do
!
      end subroutine read_rtp_gl_1d_table
!
! -----------------------------------------------------------------------
!
      subroutine read_rj_gl_1d_table(id_file, sph_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      sph_IO%numdir_sph = 2
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 3
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%nidx_sph(1),                      &
     &                         sph_IO%ist_sph(1), sph_IO%ied_sph(1)
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(1)
        read(id_file,*) sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i)
      end do
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%nidx_sph(2),                      &
     &                         sph_IO%ist_sph(2), sph_IO%ied_sph(2)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(2)
        read(id_file,*) sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2))
      end do
!
      end subroutine read_rj_gl_1d_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rtp_gl_1d_table(id_file, sph_IO)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)', advance='NO') hd_rgrid()
      write(id_file,'(3i16)') sph_IO%nidx_sph(1),                       &
     &                        sph_IO%ist_sph(1), sph_IO%ied_sph(1)
      do i = 1, sph_IO%nidx_sph(1)
        write(id_file,'(i16,1pE25.15e3)')                               &
     &                        sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i)
      end do
!
      write(id_file,'(a)', advance='NO') hd_tgrid()
      write(id_file,'(3i16)') sph_IO%nidx_sph(2),                       &
     &                        sph_IO%ist_sph(2), sph_IO%ied_sph(2)
      do i = 1, sph_IO%nidx_sph(2)
        write(id_file,'(8i16)')                                         &
     &         sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2))
      end do
!
      write(id_file,'(a)', advance='NO') hd_pgrid()
      write(id_file,'(3i16)') sph_IO%nidx_sph(3),                       &
     &                        sph_IO%ist_sph(3), sph_IO%ied_sph(3)
      do i = 1, sph_IO%nidx_sph(3)
        write(id_file,'(8i16)')                                         &
     &         sph_IO%idx_gl_3(i,1:sph_IO%ncomp_table_1d(3))
      end do
!
      end subroutine write_rtp_gl_1d_table
!
! ----------------------------------------------------------------------
!
      subroutine write_rj_gl_1d_table(id_file, sph_IO)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)', advance='NO') hd_rgrid()
      write(id_file,'(3i16)') sph_IO%nidx_sph(1),                       &
     &                        sph_IO%ist_sph(1), sph_IO%ied_sph(1)
      do i = 1, sph_IO%nidx_sph(1)
        write(id_file,'(i16,1pE25.15e3)')                               &
     &                        sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i)
      end do
!
      write(id_file,'(a)', advance='NO') hd_jmode()
      write(id_file,'(3i16)') sph_IO%nidx_sph(2),                       &
     &                        sph_IO%ist_sph(2), sph_IO%ied_sph(2)
      do i = 1, sph_IO%nidx_sph(2)
        write(id_file,'(8i16)')                                         &
     &          sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2))
      end do
!
      end subroutine write_rj_gl_1d_table
!
! ----------------------------------------------------------------------
!
      end module sph_global_1d_idx_IO
