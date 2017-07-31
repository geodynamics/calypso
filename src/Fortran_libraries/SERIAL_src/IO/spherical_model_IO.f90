!>@file  spherical_model_IO.f90
!!       module spherical_model_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rank_4_sph(id_file, sph_IO)
!!      subroutine read_gl_resolution_sph(id_file, sph_IO)
!!      subroutine read_gl_nodes_sph(id_file, sph_IO)
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine write_rank_4_sph(id_file, sph_IO)
!!      subroutine write_gl_resolution_sph(id_file, sph_IO)
!!      subroutine write_gl_nodes_sph(id_file, sph_IO)
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module spherical_model_IO
!
      use m_precision
!
      use t_node_id_spherical_IO
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rank_4_sph(id_file, sph_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%sph_rank(1:sph_IO%numdir_sph)
!
      end subroutine read_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph(id_file, sph_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%nidx_gl_sph(1:sph_IO%numdir_sph)
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%ltr_gl
!
      end subroutine read_gl_resolution_sph
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph(id_file, sph_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%numnod_sph
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO%inod_gl_sph(1),                   &
     &                         sph_IO%idx_gl_sph(1,1:sph_IO%numdir_sph)
      do i = 2, sph_IO%numnod_sph
        read(id_file,*) sph_IO%inod_gl_sph(i),                          &
     &                  sph_IO%idx_gl_sph(i,1:sph_IO%numdir_sph)
      end do
!
      end subroutine read_gl_nodes_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph(id_file, sph_IO)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      write(id_file,'(a)', advance='NO') hd_segment()
      write(id_file,'(10i16)') sph_IO%sph_rank(1:sph_IO%numdir_sph)
!
      end subroutine write_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph(id_file, sph_IO)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      write(id_file,'(a)', advance='NO') hd_trunc()
      write(id_file,'(3i16)') sph_IO%nidx_gl_sph(1:sph_IO%numdir_sph)
      write(id_file,'(i16)') sph_IO%ltr_gl
!
      end subroutine write_gl_resolution_sph
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_nodes_sph(id_file, sph_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(i16)') sph_IO%numnod_sph
      do i = 1, sph_IO%numnod_sph
        write(id_file,'(20i16)') sph_IO%inod_gl_sph(i),                &
     &                sph_IO%idx_gl_sph(i,1:sph_IO%numdir_sph)
      end do
!
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine write_gl_nodes_sph
!
! -----------------------------------------------------------------------
!
      end module spherical_model_IO
