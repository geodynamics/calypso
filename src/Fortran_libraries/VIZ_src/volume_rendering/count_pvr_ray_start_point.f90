!>@file   count_pvr_ray_start_point.f90
!!@brief  module count_pvr_ray_start_point
!!
!!@author H. Matsui
!!@date Programmed in  Aug., 2011
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine count_temporal_pvr_ray_start                         &
!!     &         (num_pvr_surf, screen_norm_pvr_domain,                 &
!!     &          isurf_xrng_pvr_domain, jsurf_yrng_pvr_domain,         &
!!     &          ray_vec4, ntot_tmp_pvr_ray_sf, istack_tmp_pvr_ray_st)
!!      subroutine count_each_pvr_ray_start(node, surf,                 &
!!     &         modelview_mat, projection_mat, npixel_x, npixel_y,     &
!!     &         pixel_point_x, pixel_point_y,num_pvr_surf,             &
!!     &         item_pvr_surf_domain, screen_norm_pvr_domain,          &
!!     &         isurf_xrng_pvr_domain, jsurf_yrng_pvr_domain, ray_vec4,&
!!     &         num_pvr_ray, istack_pvr_ray_sf, ntot_tmp_pvr_ray,      &
!!     &         istack_tmp_pvr_ray_st, ipix_start_tmp, iflag_start_tmp,&
!!     &         xi_pvr_start_tmp)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!@endverbatim
!
      module count_pvr_ray_start_point
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_geometry_constants
      use t_control_params_4_pvr
!
      implicit  none
!
      private :: cal_coefs_on_surf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_temporal_pvr_ray_start                           &
     &         (num_pvr_surf, screen_norm_pvr_domain,                   &
     &          isurf_xrng_pvr_domain, jsurf_yrng_pvr_domain,           &
     &          ray_vec4, ntot_tmp_pvr_ray_sf, istack_tmp_pvr_ray_st)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: isurf_xrng_pvr_domain(2,num_pvr_surf)
      integer(kind = kint), intent(in)                                  &
     &                    :: jsurf_yrng_pvr_domain(2,num_pvr_surf)
      real(kind = kreal), intent(in)                                    &
     &                    :: screen_norm_pvr_domain(3,num_pvr_surf)
!
      real(kind = kreal), intent(in) :: ray_vec4(4)
!
      integer(kind = kint), intent(inout) :: ntot_tmp_pvr_ray_sf
      integer(kind = kint), intent(inout)                               &
     &                   :: istack_tmp_pvr_ray_st(0:num_pvr_surf)
!
      integer(kind = kint) :: inum
!
!
!$omp parallel do private(inum)
      do inum = 1, num_pvr_surf
        if((screen_norm_pvr_domain(3,inum)*ray_vec4(3))                 &
     &       .gt. SMALL_NORM) then
          istack_tmp_pvr_ray_st(inum) = (isurf_xrng_pvr_domain(2,inum)  &
     &                            - isurf_xrng_pvr_domain(1,inum)+1)    &
     &                           * (jsurf_yrng_pvr_domain(2,inum)       &
     &                            - jsurf_yrng_pvr_domain(1,inum)+1)
        else
          istack_tmp_pvr_ray_st(inum) = 0
        end if
      end do
!$omp end parallel do
!
      istack_tmp_pvr_ray_st(0) = 0
      do inum = 1, num_pvr_surf
        istack_tmp_pvr_ray_st(inum) = istack_tmp_pvr_ray_st(inum-1)     &
     &                               + istack_tmp_pvr_ray_st(inum)
      end do
      ntot_tmp_pvr_ray_sf = istack_tmp_pvr_ray_st(num_pvr_surf)
!
      end subroutine count_temporal_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine count_each_pvr_ray_start(node, surf,                   &
     &         modelview_mat, projection_mat, npixel_x, npixel_y,       &
     &         pixel_point_x, pixel_point_y,num_pvr_surf,               &
     &         item_pvr_surf_domain, screen_norm_pvr_domain,            &
     &         isurf_xrng_pvr_domain, jsurf_yrng_pvr_domain, ray_vec4,  &
     &         num_pvr_ray, istack_pvr_ray_sf, ntot_tmp_pvr_ray,        &
     &         istack_tmp_pvr_ray_st, ipix_start_tmp, iflag_start_tmp,  &
     &         xi_pvr_start_tmp)
!
      use t_geometry_data
      use t_surface_data
      use set_position_pvr_screen
      use cal_fline_in_cube
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
!
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: projection_mat(4,4)
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      real(kind = kreal), intent(in) :: pixel_point_x(npixel_x)
      real(kind = kreal), intent(in) :: pixel_point_y(npixel_y)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
      real(kind = kreal), intent(in)                                    &
     &                    :: screen_norm_pvr_domain(3,num_pvr_surf)
      integer(kind = kint), intent(in)                                  &
     &                    :: isurf_xrng_pvr_domain(2,num_pvr_surf)
      integer(kind = kint), intent(in)                                  &
     &                    :: jsurf_yrng_pvr_domain(2,num_pvr_surf)
!
      real(kind = kreal), intent(in) :: ray_vec4(4)
!
      integer(kind = kint), intent(in) :: ntot_tmp_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                   :: istack_tmp_pvr_ray_st(0:num_pvr_surf)
!
      integer(kind = kint), intent(inout) :: num_pvr_ray
      integer(kind = kint), intent(inout)                               &
     &                   :: iflag_start_tmp(ntot_tmp_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                   :: ipix_start_tmp(2,ntot_tmp_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xi_pvr_start_tmp(2,ntot_tmp_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                   :: istack_pvr_ray_sf(0:num_pvr_surf)
!
      integer(kind = kint) :: inum, iele, k1, isurf, icou
      integer(kind = kint) :: ist_pix, ied_pix, jst_pix, jed_pix
      integer(kind = kint) :: ipix, jpix, iflag
      real(kind = kreal) :: xx4_model_sf(4,num_linear_sf,nsurf_4_ele)
      real(kind = kreal) :: x_pix(2), xi(2)
!
      real(kind = kreal) :: xt1(2), a(2,2)
      real(kind = kreal) :: c1(3), c3(3), aj
!
!
!$omp parallel do private(inum,iele,k1,isurf,xx4_model_sf,iflag,        &
!$omp&        icou,ist_pix,ied_pix,jst_pix,jed_pix,ipix,jpix,x_pix,xi,  &
!$omp&        xt1,a,c1,c3,aj)
      do inum = 1, num_pvr_surf
        istack_pvr_ray_sf(inum) = 0
        iele = item_pvr_surf_domain(1,inum)
        k1 =   item_pvr_surf_domain(2,inum)
        isurf = abs(surf%isf_4_ele(iele,k1))
        icou = istack_tmp_pvr_ray_st(inum-1)
!
        if((screen_norm_pvr_domain(3,inum)*ray_vec4(3))                 &
     &       .gt. SMALL_NORM) then
          call position_on_each_ele_sfs_wone                            &
     &       (surf, node%numnod, node%xx, iele, xx4_model_sf)
          call project_once_each_element(modelview_mat, projection_mat, &
     &        (num_linear_sf*nsurf_4_ele), xx4_model_sf(1,1,1))
!
          ist_pix = isurf_xrng_pvr_domain(1,inum)
          ied_pix = isurf_xrng_pvr_domain(2,inum)
          jst_pix = jsurf_yrng_pvr_domain(1,inum)
          jed_pix = jsurf_yrng_pvr_domain(2,inum)
          do ipix = ist_pix, ied_pix
            do jpix = jst_pix, jed_pix
              icou = icou + 1
              x_pix(1) = pixel_point_x(ipix)
              x_pix(2) = pixel_point_y(jpix)
              ipix_start_tmp(1,icou) = ipix
              ipix_start_tmp(2,icou) = jpix
!
              xt1(1:2)= x_pix(1:2) - xx4_model_sf(1:2,1,k1)
              a(1:2,1)= xx4_model_sf(1:2,2,k1) - xx4_model_sf(1:2,1,k1)
              a(1:2,2)= xx4_model_sf(1:2,4,k1) - xx4_model_sf(1:2,1,k1)
              aj = one / (a(1,1)*a(2,2) - a(2,1)*a(1,2))
!
              c1(1) = ( a(2,2)*xt1(1) - a(1,2)*xt1(2) ) * aj
              c1(2) = (-a(2,1)*xt1(1) + a(1,1)*xt1(2) ) * aj
!
              xt1(1:2) = x_pix(1:2) - xx4_model_sf(1:2,3,k1)
              a(1:2,1) = xx4_model_sf(1:2,2,k1) - xx4_model_sf(1:2,3,k1)
              a(1:2,2) = xx4_model_sf(1:2,4,k1) - xx4_model_sf(1:2,3,k1)
              aj = one / (a(1,1)*a(2,2) - a(2,1)*a(1,2))
!
              c3(1) = ( a(2,2)*xt1(1) - a(1,2)*xt1(2) ) * aj
              c3(2) = (-a(2,1)*xt1(1) + a(1,1)*xt1(2) ) * aj
!
              c1(3) = one - (c1(1) + c1(2))
              c3(3) = one - (c3(1) + c3(2))
!
              if(c1(1).ge.zero .and. c1(2).ge.zero                      &
     &                         .and. c1(3).ge.zero) then
                iflag = 1
                xi(1) = -one + two*c1(1)
                xi(2) = -one + two*c1(2)
!
              else if(c3(1).ge.zero .and. c3(2).ge.zero                 &
     &                              .and. c3(3).ge.zero) then
                iflag = 1
                xi(1) = one - two*c3(2)
                xi(2) = one - two*c3(1)
              else
                xi(1) = -two
                xi(2) = -two
                iflag = 0
              end if
!              write(200+my_rank,*) 'cal_coefs_on_surf', iflag, xi(1:2),&
!     &        c1, c3
!
!
!              write(200+my_rank,*) inum, ipix, jpix, iflag, xi,        &
!     &               x_pix, xx4_model_sf(:,:,k1)
              iflag_start_tmp(icou) = iflag
              xi_pvr_start_tmp(1:2,icou) = xi
!
!              write(100+my_rank,*) inum, ipix, jpix, &
!     &             iflag_start_tmp(icou), xi_pvr_start_tmp(1:2,icou), &
!     &             x_pix, xx4_model_sf(:,:,k1)
!
              if(iflag_start_tmp(icou) .gt. 0) then
                istack_pvr_ray_sf(inum) = istack_pvr_ray_sf(inum) + 1
              end if
            end do
          end do
        end if
      end do
!$omp end parallel do
!
      istack_pvr_ray_sf(0) = 0
      do inum = 1, num_pvr_surf
        istack_pvr_ray_sf(inum) = istack_pvr_ray_sf(inum-1)             &
     &                           + istack_pvr_ray_sf(inum)
      end do
      num_pvr_ray = istack_pvr_ray_sf(num_pvr_surf)
!
      end subroutine count_each_pvr_ray_start
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_coefs_on_surf(x_surf, x_pix, iflag, xi)
!
      real(kind = kreal), intent(in) ::    x_pix(2)
      real(kind = kreal), intent(in) ::    x_surf(2,4)
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: xi(2)
!
      real(kind = kreal) :: xt1(2), a(2,2)
      real(kind = kreal) :: c1(3), c3(3), aj
!
!
      xt1(1:2) = x_pix(1:2) - x_surf(1:2,1)
      a(1:2,1) = x_surf(1:2,2) - x_surf(1:2,1)
      a(1:2,2) = x_surf(1:2,4) - x_surf(1:2,1)
      aj = one / (a(1,1)*a(2,2) - a(2,1)*a(1,2))
!
      c1(1) = ( a(2,2)*xt1(1) - a(1,2)*xt1(2) ) * aj
      c1(2) = (-a(2,1)*xt1(1) + a(1,1)*xt1(2) ) * aj
!
      xt1(1:2) = x_pix(1:2) - x_surf(1:2,3)
      a(1:2,1) = x_surf(1:2,2) - x_surf(1:2,3)
      a(1:2,2) = x_surf(1:2,4) - x_surf(1:2,3)
      aj = one / (a(1,1)*a(2,2) - a(2,1)*a(1,2))
!
      c3(1) = ( a(2,2)*xt1(1) - a(1,2)*xt1(2) ) * aj
      c3(2) = (-a(2,1)*xt1(1) + a(1,1)*xt1(2) ) * aj
!
      c1(3) = one - (c1(1) + c1(2))
      c3(3) = one - (c3(1) + c3(2))
!
      if(c1(1).ge.zero .and. c1(2).ge.zero .and. c1(3).ge.zero) then
        iflag = 1
        xi(1) = -one + two*c1(1)
        xi(2) = -one + two*c1(2)
!
      else if(c3(1).ge.zero .and. c3(2).ge.zero .and. c3(3).ge.zero)    &
     &    then
        iflag = 1
        xi(1) = one - two*c3(2)
        xi(2) = one - two*c3(1)
      else
        xi(1) = -two
        xi(2) = -two
        iflag = 0
      end if
      write(200+my_rank,*) 'cal_coefs_on_surf', iflag, xi(1:2), &
     &        c1, c3
!
      end subroutine cal_coefs_on_surf
!
!-----------------------------------------------------------------------
!
      end module count_pvr_ray_start_point
