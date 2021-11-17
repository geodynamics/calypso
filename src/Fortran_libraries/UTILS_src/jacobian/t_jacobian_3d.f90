!>@file  t_jacobian_3d.f90
!!       module t_jacobian_3d
!!
!!@author H. Matsui
!!@date   Programmed on Dec., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief  Structure of 3D Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine alloc_jacobians                                      &
!!     &         (numele, nnod_4_ele, ntot_int_3d, jac_3d)
!!      subroutine alloc_inv_jacobian(numele, jac_3d)
!!      subroutine alloc_dxi_dx(numele, jac_3d)
!!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!!        integer(kind = kint), intent(in) :: ntot_int_3d
!!        type(jacobians_3d), intent(inout) :: jac_3d
!!
!!      subroutine dealloc_jacobians(jac_3d)
!!      subroutine dealloc_inv_jacobian(jac_3d)
!!      subroutine dealloc_dxi_dx(jac_3d)
!!
!!      subroutine copy_jacobians_3d(jac_org, jac_new)
!!      subroutine copy_shape_func_infty(jac_org, jac_new)
!!      subroutine copy_dxidx_3d(jac_org, jac_new)
!!
!!  definision of matrix
!!         dxidx_3d(iele,ix,1,1) :: dxi / dx
!!         dxidx_3d(iele,ix,2,1) :: dei / dx
!!         dxidx_3d(iele,ix,3,1) :: dzi / dx
!!
!!         dxidx_3d(iele,ix,1,2) :: dxi / dy
!!         dxidx_3d(iele,ix,2,2) :: dei / dy
!!         dxidx_3d(iele,ix,3,2) :: dzi / dy
!!
!!         dxidx_3d(iele,ix,1,3) :: dxi / dz
!!         dxidx_3d(iele,ix,2,3) :: dei / dz
!!         dxidx_3d(iele,ix,3,3) :: dzi / dz
!!
!!         iele: element ID
!!         ix:   integration point ID
!!@endverbatim
!>@n @param   ntot_int_3d
!>     Total number of integration point for 3D element
!
!>@n @param   an(Shape_function_ID,integration_point)
!>      Shape function at integration point for linear element
!>@n      \f[ N_\alpha(\xi,\chi,\eta) \f]
!
!>@n @param   dnx(element_ID,Shape_function_ID,integration_point,direction)
!>      Spatial differnce of linear shape function at integration
!>      point for element
!>@n      \f[ \frac{ dN_\alpha(\xi,\chi,\eta) }{ dx}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dy}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dz} \f]
!
!>@n @param    an_infty(Shape_function_ID,surface_ID,integration_point)
!>      Shape function at integration point for linear infinity element
!>@n      \f[ N_{\infty\alpha}(\xi,\chi,\eta) \f]
!
!>@n @param   xjac(element_ID,integration_point)
!>      Jacobian at integration point for linear element
!>@n      \f[ Ja = \det \left(\frac{ d{\bf x} }{ d {\bf \xi}} \right)\f]
!>@n @param   axjac(element_ID,integration_point)
!>        \f[ Ja^{-1}\f]
!
!>
!>@n @param   aw(Shape_function_ID,integration_point)
!>      Shape function at integration point for element
!>@n      \f[ N_\alpha(\xi,\chi,\eta) \f]
!
!>@n @param   dwx(element_ID,Shape_function_ID,integration_point,direction)
!>      Spatial differnce of shape function at integration
!>      point for element
!>@n      \f[ \frac{ dN_\alpha(\xi,\chi,\eta) }{ dx}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dy}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dz} \f]
!
!>@n @param    aw_infty(Shape_function_ID,surface_ID,integration_point)
!>      Shape function at integration point for infinity element
!>@n      \f[ N_{\infty\alpha}(\xi,\chi,\eta) \f]
!
!>@n @param   xjac_q(element_ID,integration_point)
!>      Jacobian at integration point for element
!>@n      \f[ Ja = \det \left(\frac{ d{\bf x} }{ d {\bf \xi}} \right)\f]
!>@n @param   axjac_q(element_ID,integration_point)
!>        \f[ Ja^{-1}\f]
!
!>
!>@n @param   am(Shape_function_ID,integration_point)
!>      Shape function at integration point for element
!>@n      \f[ N_\alpha(\xi,\chi,\eta) \f]
!
!>@n @param   dmx(element_ID,Shape_function_ID,integration_point,direction)
!>      Spatial differnce of shape function at integration
!>      point for element
!>@n      \f[ \frac{ dN_\alpha(\xi,\chi,\eta) }{ dx}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dy}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dz} \f]
!
!>@n @param    am_infty(Shape_function_ID,surface_ID,integration_point)
!>      Shape function at integration point for infinity element
!>@n      \f[ N_{\infty\alpha}(\xi,\chi,\eta) \f]
!
!>@n @param   xjac_lq(element_ID,integration_point)
!>      Jacobian at integration point for element
!>@n      \f[ Ja = \det \left(\frac{ d{\bf x} }{ d {\bf \xi}} \right)\f]
!>@n @param   axjac_lq(element_ID,integration_point)
!>        \f[ Ja^{-1}\f]
!
      module t_jacobian_3d
!
      use m_precision
!
      implicit  none
!
!>     Stracture for Jacobians for element
      type jacobians_3d
!>   Total number of integration points
        integer(kind = kint) :: ntot_int
!>   Shape function at integration points
        real (kind=kreal), allocatable :: an(:,:)
!>   Spatial differnce of Shape function  at integration points
        real (kind=kreal), allocatable :: dnx(:,:,:,:)
!
!>   Shape function for infinite element at integration points
        real (kind=kreal), allocatable :: an_infty(:,:,:)
!
!>   Jacobian at integration points
        real (kind=kreal), allocatable :: xjac(:,:)
!>   1 / Jacbian
        real (kind=kreal), allocatable :: axjac(:,:)
!
!>   dxi / dx
        real(kind=kreal),   allocatable :: dxidx_3d(:,:,:,:)
      end type jacobians_3d
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_jacobians                                        &
     &         (numele, nnod_4_ele, ntot_int_3d, jac_3d)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ntot_int_3d
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      jac_3d%ntot_int = ntot_int_3d
      allocate(jac_3d%an(nnod_4_ele,jac_3d%ntot_int))
      allocate(jac_3d%an_infty(nnod_4_ele,nsurf_4_ele,jac_3d%ntot_int))
!
      allocate(jac_3d%dnx(numele,nnod_4_ele,jac_3d%ntot_int,3))
!
      allocate(jac_3d%xjac(numele,jac_3d%ntot_int))
!
      jac_3d%an = 0.0d0
      jac_3d%an_infty = 0.0d0
!
      if (numele .gt. 0) then
        jac_3d%dnx = 0.0d0
!
        jac_3d%xjac = 0.0d0
      end if
!
      end subroutine alloc_jacobians
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_inv_jacobian(numele, jac_3d)
!
      integer(kind = kint), intent(in) :: numele
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      allocate(jac_3d%axjac(numele,jac_3d%ntot_int))
      if(numele .gt. 0) jac_3d%axjac = 0.0d0
!
      end subroutine alloc_inv_jacobian
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dxi_dx(numele, jac_3d)
!
      integer(kind = kint), intent(in) :: numele
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      allocate( jac_3d%dxidx_3d(numele,jac_3d%ntot_int,3,3) )
      if (numele .gt. 0) jac_3d%dxidx_3d = 0.0d0
!
      end subroutine alloc_dxi_dx
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians(jac_3d)
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
      if(allocated(jac_3d%an) .eqv. .FALSE.) return
      deallocate(jac_3d%an, jac_3d%an_infty)
      deallocate(jac_3d%dnx)
!
      deallocate(jac_3d%xjac)
!
      end subroutine dealloc_jacobians
!
!  ------------------------------------------------------------------
!
      subroutine dealloc_inv_jacobian(jac_3d)
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
      if(allocated(jac_3d%axjac) .eqv. .FALSE.) return
      deallocate(jac_3d%axjac)
!
      end subroutine dealloc_inv_jacobian
!
!  ------------------------------------------------------------------
!
      subroutine dealloc_dxi_dx(jac_3d)
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
      if(allocated(jac_3d%dxidx_3d) .eqv. .FALSE.) return
      deallocate(jac_3d%dxidx_3d)
!
      end subroutine dealloc_dxi_dx
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_jacobians_3d(jac_org, jac_new)
!
      type(jacobians_3d), intent(in) :: jac_org
      type(jacobians_3d), intent(inout) :: jac_new
!
!
       jac_new%an = jac_org%an
       jac_new%dnx = jac_org%dnx
!
       jac_new%xjac  = jac_org%xjac
       jac_new%axjac = jac_org%axjac
!
       end subroutine copy_jacobians_3d
!
!  ------------------------------------------------------------------
!
      subroutine copy_shape_func_infty(jac_org, jac_new)
!
      type(jacobians_3d), intent(in) :: jac_org
      type(jacobians_3d), intent(inout) :: jac_new
!
!
      jac_new%an_infty = jac_org%an_infty
!
      end subroutine copy_shape_func_infty
!
!  ------------------------------------------------------------------
!
      subroutine copy_dxidx_3d(jac_org, jac_new)
!
      type(jacobians_3d), intent(in) :: jac_org
      type(jacobians_3d), intent(inout) :: jac_new
!
!
      jac_new%dxidx_3d = jac_org%dxidx_3d
!
      end subroutine copy_dxidx_3d
!
!  ------------------------------------------------------------------
!
      end module t_jacobian_3d
