!> @file  bcast_file_IO_parameter.f90
!!      module bcast_file_IO_parameter
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for file information (prefix, format, IO flag)
!!
!!@verbatim
!!      subroutine bcast_field_IO_parameter(file_param)
!!        type(field_IO_params), intent(inout) :: file_param
!!      subroutine bcast_FEM_file_IO_flags(FEM_mesh_flags)
!!        type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!!@endverbatim
!
      module bcast_file_IO_parameter
!
      use m_precision
      use m_constants
      use t_file_IO_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_field_IO_parameter(file_param)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(field_IO_params), intent(inout) :: file_param
!
!
      call calypso_mpi_bcast_character                                  &
     &   (file_param%file_prefix, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(file_param%iflag_IO , 0)
      call calypso_mpi_bcast_one_int(file_param%iflag_format, 0)
!
      end subroutine bcast_field_IO_parameter
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_FEM_file_IO_flags(FEM_mesh_flags)
!
      use calypso_mpi_int
!
      type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!
!
      call calypso_mpi_bcast_one_int                                    &
     &   (FEM_mesh_flags%iflag_access_FEM, 0)
      call calypso_mpi_bcast_one_int                                    &
     &   (FEM_mesh_flags%iflag_output_SURF , 0)
      call calypso_mpi_bcast_one_int                                    &
     &   (FEM_mesh_flags%iflag_output_VMESH, 0)
!
      end subroutine bcast_FEM_file_IO_flags
!
!  ---------------------------------------------------------------------
!
      end module bcast_file_IO_parameter
