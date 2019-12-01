!>@file   t_boundary_sph_spectr.f90
!!@brief  module t_boundary_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!!
!!@verbatim
!!      subroutine alloc_sph_scalar_bcs_data(jmax, bcs_S)
!!      subroutine dealloc_sph_scalar_bcs_data(bcs_S)
!!        type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!!      subroutine alloc_sph_vector_bcs_data(jmax, bcs_V)
!!      subroutine dealloc_sph_vector_bcs_data(bcs_V)
!!        type(sph_vector_boundary_data), intent(inout) :: bcs_V
!!@endverbatim
!!
!!@n @param jmax    number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param nri     number of radial grid points
!!@n @param radius  radius
!
      module t_boundary_sph_spectr
!
      use m_precision
!
      implicit none
!
!
!>      Structure for boundary scalar spectr
      type sph_scalar_BC_coef
!>        Number of componentts
        integer(kind = kint) :: jmax_sBC
!
!>        Fixed poloidal velocity spectrum for center
        real(kind = kreal) :: S_CTR = 0.0d0
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: S_BC(:)
      end type sph_scalar_BC_coef
!
!>      Structure for boundary velocity spectr
      type sph_vector_BC_coef
!>        Number of componentts
        integer(kind = kint) :: jmax_vBC
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vp_BC(:)
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Dp_BC(:)
!>        Fixed toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vt_BC(:)
      end type sph_vector_BC_coef
!
!
!>      Structure for boundary scalar spectr
      type sph_scalar_BC_evo
!>        Number of componentts
        integer(kind = kint) :: jmax_sBC
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: S_BC_mag(:)
!>        Angular frequency scalar spectrum for ICB
        real(kind = kreal), allocatable :: S_BC_freq(:)
!>        Angular phase scalar spectrum for ICB
        real(kind = kreal), allocatable :: S_BC_phase(:)
      end type sph_scalar_BC_evo
!
!
!>      Structure for boundary velocity spectr
      type sph_vector_BC_evo
!>        Number of componentts
        integer(kind = kint) :: jmax_vBC
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vp_BC_mag(:)
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Dp_BC_mag(:)
!>        Fixed toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vt_BC_mag(:)
!
!>        Angular phase of poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vp_BC_phase(:)
!>        Angular phase of poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Dp_BC_phase(:)
!>        Angular phase of toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vt_BC_phase(:)
!
!>        Angular frequency poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vp_BC_freq(:)
!>        Angular frequency toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vt_BC_freq(:)
      end type sph_vector_BC_evo
!
!
!>      Structure for scalar spectr boundaries
      type sph_scalar_boundary_data
!>        Structure for boundary scalar spectr
        type(sph_scalar_BC_coef) :: ICB_Sspec
!>        Structure for boundary scalar spectr
        type(sph_scalar_BC_coef) :: CMB_Sspec
!>        Structure for evoluved boundary scalar spectr
        type(sph_scalar_BC_evo) :: ICB_Sevo
!>        Structure for evoluved boundary scalar spectr
        type(sph_scalar_BC_evo) :: CMB_Sevo
      end type sph_scalar_boundary_data
!
!>      Structure for vector spectr boundaries
      type sph_vector_boundary_data
!>        Structure for boundary scalar spectr
        type(sph_vector_BC_coef) :: ICB_Vspec
!>        Structure for boundary scalar spectr
        type(sph_vector_BC_coef) :: CMB_Vspec
!>        Structure for evoluved boundary scalar spectr
        type(sph_vector_BC_evo) :: ICB_Vevo
!>        Structure for evoluved boundary scalar spectr
        type(sph_vector_BC_evo) :: CMB_Vevo
      end type sph_vector_boundary_data
!
      private :: alloc_sph_scalar_bc_array
      private :: alloc_sph_evo_scalar_bc_array
      private :: dealloc_sph_scalar_bc_array
      private :: dealloc_sph_evo_scalar_bc_array
      private :: alloc_sph_vector_bc_array
      private :: alloc_sph_evo_vector_bc_array
      private :: dealloc_sph_vector_bc_array
      private :: dealloc_sph_evo_vector_bc_array
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_scalar_bcs_data(jmax, bcs_S)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!
!
      call alloc_sph_scalar_bc_array(jmax, bcs_S%ICB_Sspec)
      call alloc_sph_scalar_bc_array(jmax, bcs_S%CMB_Sspec)
!
      call alloc_sph_evo_scalar_bc_array(jmax, bcs_S%ICB_Sevo)
      call alloc_sph_evo_scalar_bc_array(jmax, bcs_S%CMB_Sevo)
!
      end subroutine alloc_sph_scalar_bcs_data
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_vector_bcs_data(jmax, bcs_V)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_vector_boundary_data), intent(inout) :: bcs_V
!
!
      call alloc_sph_vector_bc_array(jmax, bcs_V%ICB_Vspec)
      call alloc_sph_vector_bc_array(jmax, bcs_V%CMB_Vspec)
      call alloc_sph_evo_vector_bc_array(jmax, bcs_V%ICB_Vevo)
      call alloc_sph_evo_vector_bc_array(jmax, bcs_V%CMB_Vevo)
!
      end subroutine alloc_sph_vector_bcs_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_scalar_bcs_data(bcs_S)
!
      type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!
!
      call dealloc_sph_scalar_bc_array(bcs_S%ICB_Sspec)
      call dealloc_sph_scalar_bc_array(bcs_S%CMB_Sspec)
!
      call dealloc_sph_evo_scalar_bc_array(bcs_S%ICB_Sevo)
      call dealloc_sph_evo_scalar_bc_array(bcs_S%CMB_Sevo)
!
      end subroutine dealloc_sph_scalar_bcs_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_vector_bcs_data(bcs_V)
!
      type(sph_vector_boundary_data), intent(inout) :: bcs_V
!
!
      call dealloc_sph_vector_bc_array(bcs_V%ICB_Vspec)
      call dealloc_sph_vector_bc_array(bcs_V%CMB_Vspec)
      call dealloc_sph_evo_vector_bc_array(bcs_V%ICB_Vevo)
      call dealloc_sph_evo_vector_bc_array(bcs_V%CMB_Vevo)
!
      end subroutine dealloc_sph_vector_bcs_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_scalar_bc_array(jmax, bc_Sspec)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!
!
      bc_Sspec%jmax_sBC = jmax
      allocate(bc_Sspec%S_BC(bc_Sspec%jmax_sBC))
!
      if(bc_Sspec%jmax_sBC .le. 0) return
      bc_Sspec%S_BC = 0.0d0
!
      end subroutine alloc_sph_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_vector_bc_array(jmax, bc_Vspec)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!
!
      bc_Vspec%jmax_vBC = jmax
!
      allocate(bc_Vspec%Vp_BC(bc_Vspec%jmax_vBC))
      allocate(bc_Vspec%Dp_BC(bc_Vspec%jmax_vBC))
      allocate(bc_Vspec%Vt_BC(bc_Vspec%jmax_vBC))
!
      if(bc_Vspec%jmax_vBC .le. 0) return
      bc_Vspec%Vp_BC = 0.0d0
      bc_Vspec%Dp_BC = 0.0d0
      bc_Vspec%Vt_BC = 0.0d0
!
      end subroutine alloc_sph_vector_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_evo_scalar_bc_array(jmax, bc_Sevo)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
!
      bc_Sevo%jmax_sBC = jmax
!
      allocate(bc_Sevo%S_BC_mag(bc_Sevo%jmax_sBC))
      allocate(bc_Sevo%S_BC_freq(bc_Sevo%jmax_sBC))
      allocate(bc_Sevo%S_BC_phase(bc_Sevo%jmax_sBC))
!
      if(bc_Sevo%jmax_sBC .le. 0) return
      bc_Sevo%S_BC_mag = 0.0d0
      bc_Sevo%S_BC_freq = 0.0d0
      bc_Sevo%S_BC_phase = 0.0d0
!
      end subroutine alloc_sph_evo_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_evo_vector_bc_array(jmax, bc_Vevo)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!
!
      bc_Vevo%jmax_vBC = jmax
!
      allocate(bc_Vevo%Vp_BC_mag(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Dp_BC_mag(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Vt_BC_mag(bc_Vevo%jmax_vBC))
!
      allocate(bc_Vevo%Vp_BC_phase(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Dp_BC_phase(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Vt_BC_phase(bc_Vevo%jmax_vBC))
!
      allocate(bc_Vevo%Vp_BC_freq(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Vt_BC_freq(bc_Vevo%jmax_vBC))
!
      if(bc_Vevo%jmax_vBC .le. 0) return
      bc_Vevo%Vp_BC_mag = 0.0d0
      bc_Vevo%Dp_BC_mag = 0.0d0
      bc_Vevo%Vt_BC_mag = 0.0d0
!
      bc_Vevo%Vp_BC_freq = 0.0d0
      bc_Vevo%Vt_BC_freq = 0.0d0
!
      bc_Vevo%Vp_BC_phase = 0.0d0
      bc_Vevo%Dp_BC_phase = 0.0d0
      bc_Vevo%Vt_BC_phase = 0.0d0
!
      end subroutine alloc_sph_evo_vector_bc_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_scalar_bc_array(bc_Sspec)
!
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!
      deallocate(bc_Sspec%S_BC)
!
      end subroutine dealloc_sph_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_vector_bc_array(bc_Vspec)
!
      type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!
      deallocate(bc_Vspec%Vp_BC, bc_Vspec%Dp_BC, bc_Vspec%Vt_BC)
!
      end subroutine dealloc_sph_vector_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_evo_scalar_bc_array(bc_Sevo)
!
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
!
      deallocate(bc_Sevo%S_BC_mag)
      deallocate(bc_Sevo%S_BC_phase, bc_Sevo%S_BC_freq)
!
      end subroutine dealloc_sph_evo_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_evo_vector_bc_array(bc_Vevo)
!
      type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!
!
      deallocate(bc_Vevo%Vp_BC_mag, bc_Vevo%Dp_BC_mag)
      deallocate(bc_Vevo%Vt_BC_mag)
!
      deallocate(bc_Vevo%Vp_BC_freq,  bc_Vevo%Vt_BC_freq)
!
      deallocate(bc_Vevo%Vp_BC_phase, bc_Vevo%Dp_BC_phase)
      deallocate(bc_Vevo%Vt_BC_phase)
!
      end subroutine dealloc_sph_evo_vector_bc_array
!
! -----------------------------------------------------------------------
!
      end module t_boundary_sph_spectr
