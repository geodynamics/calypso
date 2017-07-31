!>@file  gz_MPI_spherical_model_IO_b.f90
!!       module gz_MPI_spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_mpi_read_rank_4_sph_b(IO_param, sph_IO)
!!      subroutine gz_mpi_read_gl_reso_sph_b(IO_param, sph_IO)
!!      subroutine gz_mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine gz_mpi_write_rank_4_sph_b(IO_param, sph_IO)
!!      subroutine gz_mpi_write_gl_reso_sph_b(IO_param, sph_IO)
!!      subroutine gz_mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module gz_MPI_spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_rank_4_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine gz_mpi_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_gl_reso_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
      call gz_mpi_read_one_inthead_b(IO_param, sph_IO%ltr_gl)
!
      end subroutine gz_mpi_read_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind=kint) :: nvect
!
!
      call gz_mpi_read_one_integer_b(IO_param, sph_IO%numnod_sph)
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (IO_param, sph_IO%numnod_sph, sph_IO%inod_gl_sph)
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_mpi_read_int_vector_b                                     &
     &   (IO_param, nvect, sph_IO%idx_gl_sph)
!
      end subroutine gz_mpi_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_rank_4_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine gz_mpi_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_gl_reso_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_mpi_write_mul_inthead_b                                   &
     &   (IO_param, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
      call gz_mpi_write_one_inthead_b(IO_param, sph_IO%ltr_gl)
!
      end subroutine gz_mpi_write_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  nvect
!
!
      call gz_mpi_write_one_integer_b(IO_param, sph_IO%numnod_sph)
      call gz_mpi_write_int8_vector_b                                   &
     &   (IO_param, sph_IO%numnod_sph, sph_IO%inod_gl_sph)
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, nvect, sph_IO%idx_gl_sph)
!
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine gz_mpi_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_spherical_model_IO_b
