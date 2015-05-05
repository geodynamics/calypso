!Author: Harsha Lokavarapu
!Date: 9/13/14

      module cuda_optimizations 
        use calypso_mpi
        use m_work_time
        use iso_c_binding
        use m_precision
        use m_spheric_parameter
        use m_spheric_param_smp
        use m_machine_parameter
        use m_schmidt_poly_on_rtm
        use schmidt_fix_m
        use m_work_4_sph_trans
        use spherical_SRs_N
        use const_coriolis_sph_rlm
        use legendre_bwd_trans_org

        implicit none

        contains
      
        subroutine alloc_mem_4_gpu(ncomp)
        integer(kind = kint), intent(in) :: ncomp
#if defined(CUDA_TIMINGS)
          call start_eleps_time(55) 
#endif
          call initgpu(nnod_rtp, nnod_rtm, nnod_rlm, nidx_rtm(1),       &
     &                      nidx_rlm(1), istep_rtm(1), istep_rlm(1),    &
     &                      ncomp, l_truncation)
#if defined(CUDA_TIMINGS)
          call end_eleps_time(55) 
#endif

#if defined(CUDA_STATIC)
#if defined(CUDA_TIMINGS)
            call start_eleps_time(60) 
#endif
          call cpy_schmidt_2_gpu(P_jl(1,1), dPdt_jl(1,1))
#if defined(CUDA_TIMINGS)
            call end_eleps_time(60) 
#endif
#endif

        call setptrs(idx_gl_1d_rlm_j(1,1))

        end subroutine alloc_mem_4_gpu

        subroutine set_mem_4_gpu
#if defined(CUDA_TIMINGS)
          call start_eleps_time(56) 
#endif

          call memcpy_h2d(lstack_rlm(0), a_r_1d_rlm_r(1),g_colat_rtm(1),&
     &                         g_sph_rlm(1,3), asin_theta_1d_rtm(1))

#if defined(CUDA_TIMINGS)
          call end_eleps_time(56) 
#endif
        end subroutine set_mem_4_gpu

        subroutine check_field_data_cuda(ncomp,nvector,nscalar,vr_rtm)
!
        integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
! Data for Base case 
        real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
        
        integer(kind = kint) :: l, m, j, l_rtm, k_rtm, nd, ip_rtm 
        integer(kind = kint) :: jst, jed, mp_rlm, j_rlm, pos
        real(kind = kreal) :: error, eps
        character(len=5) :: c_rank
        character(len=35) :: fileName        
        write(c_rank, '(I1)') my_rank

        eps = 1e-5
!       fileName = "bwd_SHT_domainId_"//c_rank//"_base.dat"         
       fileName = "cuda_calypso.dat"         
       open (unit=1, file=fileName, action="write", status="replace")
       fileName = "cuda_errors.dat"
       open (unit=2, file=fileName, action="write", status="replace")
       write(1, '(a)') 'shell,theta, vector,order, degree,      &
     &      g_sph_rlm, P_smdt, cuda, dP_smdt, cuda, vr_rtm[0], cuda,    &
     &            vr_rtm[1], cuda, vr_rtm[2], cuda'

       write(2, '(a)') 'shell, order, degree, indx_theta' 

       do k_rtm = 1, nidx_rtm(1) 
! l_trncation + 1 is s.t. m==0 and +2 is s.t. m==1
         do mp_rlm = l_truncation+2, nidx_rtm(3)
           jst = lstack_rlm(mp_rlm-1) + 1
           jed = lstack_rlm(mp_rlm)
           do l_rtm = 1, nidx_rtm(2) 
             do j_rlm = jst, jed
                 do nd = 1, nvector
                   ip_rtm = 3*nd + ncomp*(l_rtm-1)*istep_rtm(2) +      &
     &            (k_rtm-1)*istep_rtm(1) + (mp_rlm-1)*istep_rtm(3)
                   m = idx_gl_1d_rlm_j(j_rlm,3)
                   l = idx_gl_1d_rlm_j(j_rlm,2)
                   pos = (l_rtm-1)*nidx_rlm(2) + l*(l+1) + m + 1
!                   write(1,*) k_rtm, g_colat_rtm(l_rtm), nd, m, l,  &
!     &               g_sph_rlm(j_rlm,3),             &
!     &                     P_jl(j_rlm,l_rtm), P_smdt_cuda(pos),&
!     &                   dPdt_jl(j_rlm,l_rtm), dP_smdt_cuda(pos)&
!     &               , vr_rtm(ip_rtm-2), vr_rtm_cuda(ip_rtm-2),        &
!     &                vr_rtm(ip_rtm-1), vr_rtm_cuda(ip_rtm-1),          &
!     &                 vr_rtm(ip_rtm), vr_rtm_cuda(ip_rtm)
!                   error = abs(P_jl(j_rlm,l_rtm) - P_smdt_cuda(pos))
!                   if (error .GT. eps) then
!                     write(*,*) k_rtm, m, l, l_rtm
!                     write(*,*) 'ERROR:P_smdt'
!                     write(*,*) error
!                     stop 
!                   endif
!                   error = dabs(dPdt_jl(j_rlm,l_rtm) - dP_smdt_cuda(pos))
!                   if(error .GT. eps) then 
!                     write(*,*) k_rtm, m, l, l_rtm
!                     write(*,*) 'ERROR: dPdt'
!                     write(*,*) error
!                     stop 
!                   endif
!                   error = dabs(vr_rtm(ip_rtm-2)- vr_rtm_cuda(ip_rtm-2)) 
!                   if(error .GT. eps) then 
!                     write(*,*) k_rtm, m, l, l_rtm
!                     write(*,*) 'ERROR: vr_rtm[0]'
!                     write(*,*) error
!                     stop 
!                   endif
!                   error = dabs(vr_rtm(ip_rtm-1)- vr_rtm_cuda(ip_rtm-1)) 
!                   if(error .GT. eps) then 
!                     write(*,*) k_rtm, m, l, l_rtm
!                     write(*,*) 'ERROR: vr_rtm[1]'
!                     write(*,*) error
!                     stop 
!                   endif
!                   error = dabs(vr_rtm(ip_rtm)- vr_rtm_cuda(ip_rtm)) 
!                   if(error .GT. eps) then 
!                     write(*,*) k_rtm, m, l, l_rtm
!                     write(*,*) 'ERROR: vr_rtm[2]'
!                     write(*,*) error
!                     stop 
!                   endif
               end do
             end do
           end do
         end do
      end do
      close(1)
      close(2)
        end subroutine check_field_data_cuda                     

        subroutine fwd_transform(ncomp_, nvector_, nscalar_)
          integer(kind=kint), intent(in) :: ncomp_, nvector_, nscalar_
        !  call transform_f(ncomp_, nvector_, nscalar_)
        end subroutine fwd_transform

        subroutine cpy_spectrum_dat_2_gpu(ncomp, sp_rlm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
          
          call set_spectrum_data(sp_rlm(1))
        end subroutine cpy_spectrum_dat_2_gpu

       
        subroutine cpy_physical_dat_from_gpu(ncomp, vr_rtm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
          
          call retrieve_physical_data(vr_rtm(1))
        end subroutine cpy_physical_dat_from_gpu

        subroutine cpy_physical_dat_2_gpu(ncomp, vr_rtm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
          
          call set_physical_data(vr_rtm(1))
        end subroutine cpy_physical_dat_2_gpu

        subroutine finalize_gpu
          call cleangpu
        end subroutine finalize_gpu 
 
      end module cuda_optimizations 
