import * as c from 'const';
import 'services/formBindings';
import Account from 'services/Api/Account';
import { observable, action } from 'mobx';
import FormState from 'services/FormState';
import ResponseError from 'services/Api/ResponseError';
import { required, validateNewLogin, isPhone } from 'services/validators';
import routeManager from 'services/routeManager';
import { formatPhoneToServerString } from 'services/utils';

const api = Account.create();

const fields: any = {
  login: {
    name: 'login',
    label: 'Телефон или почта',
    validators: [
      required('Введите почту или номер телефона'),
      validateNewLogin,
    ],
    bindings: 'MaterialTextField',
  },
  password: {
    name: 'password',
    label: 'Пароль',
    validators: [required('Укажите пароль')],
    bindings: 'MaterialTextField',
  },
  captcha: {
    name: 'captcha',
    bindings: 'Recaptcha',
  },
};

class LoginFormState extends (FormState as any) {
  @observable public showCaptcha: boolean = false;
  @observable public startLogin: string = '';
  @observable public showText: boolean = false;
  @observable public message: string = '';

  @action
  public saveLogin = (login: string): void => {
    if (login) {
      this.$('login').set(login);
    }
  };

  @action
  public handleError = (e: ResponseError): void => {
    this.message = '';
    this.successSubmitted = false;
    switch (e.code) {
      case c.LOGIN_INVALID_CAPTCHA:
        this.showCaptcha = true;
        this.$('captcha').set('');
        this.$('captcha').invalidate('Неверная captcha');
        break;
      case c.VERIFY_USER_FAILED_CODE:
        this.message = 'Проверьте правильность написания логина и/или пароля';
        break;
      default:
        console.error(e.code, e.message);
    }
  };

  @action
  public handleResponse = (response: string): void => {
    this.message = '';
    const backUrl = localStorage.getItem('backUrl');
    if (backUrl && backUrl !== window.location.href) {
      localStorage.removeItem('backUrl');
      location.href = backUrl;
    } else {
      routeManager.goTo(response);
    }
  };

  public validateCaptcha = (captcha: string): void => {
    if (this.showCaptcha && !captcha) {
      this.$('captcha').invalidate('Captcha не подтверждена');
    }
  };

  @action
  public toggleVisibility = (): void => {
    this.showText = !this.showText;
  };

  public onError(form: any): void {
    console.log(form.errors());
  }

  public onSuccess(form: any): Promise<void> {
    const { login, password, captcha } = form.values();
    this.successSubmitted = true;
    this.validateCaptcha(captcha);
    const result = isPhone(login) ? formatPhoneToServerString(login) : login;
    return api
      .verifyUser(result, password, captcha)
      .then(this.handleResponse)
      .catch(this.handleError);
  }
}

// @ts-ignore
export const loginFormState = new LoginFormState({ fields });
