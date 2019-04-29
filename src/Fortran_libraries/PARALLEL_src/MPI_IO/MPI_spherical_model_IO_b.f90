!>@file  MPI_spherical_model_IO_b.f90
!!       module MPI_spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine mpi_read_rank_4_sph_b(IO_param, sph_IO)
!!      subroutine mpi_read_gl_reso_sph_b(IO_param, sph_IO)
!!      subroutine mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine mpi_write_rank_4_sph_b(IO_param, sph_IO)
!!      subroutine mpi_write_gl_reso_sph_b(IO_param, sph_IO)
!!      subroutine mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module MPI_spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_rank_4_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sph_IO%numdir_sph
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%sph_rank)
!
      end subroutine mpi_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_reso_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sph_IO%numdir_sph
      call mpi_read_mul_inthead_b                                       &
     &   (IO_param, num64, sph_IO%nidx_gl_sph)
      call mpi_read_one_inthead_b(IO_param, sph_IO%ltr_gl)
!
      end subroutine mpi_read_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_nodes_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) ::  num64
!
!
      call mpi_read_one_integer_b(IO_param, sph_IO%numnod_sph)
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      num64 = sph_IO%numnod_sph
      call mpi_read_int8_vector_b                                       &
     &   (IO_param, num64, sph_IO%inod_gl_sph)
!
      num64 = sph_IO%numnod_sph * sph_IO%numdir_sph
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%idx_gl_sph)
!
      end subroutine mpi_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rank_4_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint_gl) ::  num64
!
      call set_istack_4_fixed_num(sph_IO%numdir_sph, IO_param)
!
      num64 = sph_IO%numdir_sph
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%sph_rank)
!
      end subroutine mpi_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_reso_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sph_IO%numdir_sph
      call mpi_write_mul_inthead_b                                      &
     &   (IO_param, num64, sph_IO%nidx_gl_sph)
      call mpi_write_one_inthead_b(IO_param, sph_IO%ltr_gl)
!
      end subroutine mpi_write_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_nodes_sph_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint_gl) ::  num64
!
!
      call mpi_write_one_integer_b(IO_param, sph_IO%numnod_sph)
!
      num64 = sph_IO%numnod_sph
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int8_vector_b                                      &
     &   (IO_param, num64, sph_IO%inod_gl_sph)
!
      num64 = sph_IO%numnod_sph * sph_IO%numdir_sph
      call mul_istack_4_parallell_vect(sph_IO%numdir_sph, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, num64, sph_IO%idx_gl_sph)
!
      end subroutine mpi_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module MPI_spherical_model_IO_b
