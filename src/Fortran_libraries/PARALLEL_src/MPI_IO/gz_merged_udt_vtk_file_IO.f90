!gz_merged_udt_vtk_file_IO.f90
!------- module gz_merged_udt_vtk_file_IO ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!>@file  gz_merged_udt_vtk_file_IO.f90
!!       module gz_merged_udt_vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in July, 2006
!!@n      Modified  in May, 2015
!
!> @brief Output merged VTK or UCD  file usging MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_ucd_file_mpi(gzip_name, ucd, m_ucd)
!!      subroutine gz_write_ucd_phys_mpi(gzip_name, ucd, m_ucd)
!!      subroutine gz_write_ucd_grid_mpi(gzip_name, ucd, m_ucd)
!!
!!      subroutine gz_write_vtk_file_mpi(gzip_name, ucd, m_ucd)
!!      subroutine gz_write_vtk_phys_mpi(gzip_name, ucd, m_ucd)
!!      subroutine gz_write_vtk_grid_mpi(gzip_name, ucd, m_ucd)
!!@endverbatim
!
      module gz_merged_udt_vtk_file_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_field_file_format
!
      use t_ucd_data
!
      use set_ucd_file_names
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_ucd_file_mpi(gzip_name, ucd, m_ucd)
!
      use gz_ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'gzipped UCD data by MPI-IO: ', trim(gzip_name)
      end if
!
      call calypso_mpi_write_file_open(gzip_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call gz_write_ucd_mesh_mpi(id_vtk, ioff_gl, ucd%nnod, ucd%nele,   &
     &    ucd%nnod_4_ele, ucd%ntot_comp, ucd%xx, ucd%ie,                &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call gz_write_ucd_data_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine gz_write_ucd_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_phys_mpi(gzip_name, ucd, m_ucd)
!
      use gz_ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'gzipped UCD field by MPI-IO: ', trim(gzip_name)
      end if
!
      call calypso_mpi_write_file_open(gzip_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call gz_write_ucd_data_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine gz_write_ucd_phys_mpi
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_ucd_grid_mpi(gzip_name, ucd, m_ucd)
!
      use gz_ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
      if(my_rank .eq. 0) then
        write(*,*) 'gzipped UCD grid by MPI-IO: ', trim(gzip_name)
      end if
!
!
      call calypso_mpi_write_file_open(gzip_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call gz_write_ucd_mesh_mpi(id_vtk, ioff_gl, ucd%nnod, ucd%nele,   &
     &    ucd%nnod_4_ele, ucd%ntot_comp, ucd%xx, ucd%ie,                &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine gz_write_ucd_grid_mpi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_write_vtk_file_mpi(gzip_name, ucd, m_ucd)
!
      use gz_vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
     if(my_rank .eq. 0) then
        write(*,*) 'gzipped VTK data by MPI-IO: ', trim(gzip_name)
      end if
!
      call calypso_mpi_write_file_open(gzip_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call gz_write_vtk_mesh_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call gz_write_vtk_data_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine gz_write_vtk_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_phys_mpi(gzip_name, ucd, m_ucd)
!
      use gz_vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
     if(my_rank .eq. 0) then
        write(*,*) 'gzipped VTK field by MPI-IO: ', trim(gzip_name)
      end if
!
      call gz_write_vtk_phys_mpi(gzip_name, ucd, m_ucd)
!
      call calypso_mpi_write_file_open(gzip_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call gz_write_vtk_data_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine gz_write_vtk_phys_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_grid_mpi(gzip_name, ucd, m_ucd)
!
      use gz_vtk_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_barrier
      if(my_rank .eq. 0) then
        write(*,*) 'gzipped VTK grid by MPI-IO: ', trim(gzip_name)
      end if
!
      call calypso_mpi_write_file_open(gzip_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call gz_write_vtk_mesh_mpi(id_vtk, ioff_gl,                       &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine gz_write_vtk_grid_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_merged_udt_vtk_file_IO
