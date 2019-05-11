import React, { Suspense, lazy } from "react";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import Spinner from "./components/Spinner";
import NoMatch from "./containers/NoMatch";
import GlobalStyle from "./styles/GlobalStyle";
import PublicWs from "./containers/PublicWs";
import PrivateWs from "./containers/PrivateWs";

const LazyFwdAuth = lazy(() =>
  import(/*webpackChunkName: "lazyFwdAuth"*/ "./containers/FwdAuth")
);

const LazyMain = lazy(() =>
  import(/*webpackChunkName: "lazyMain"*/ "./containers/Main")
);

const LazyMenu = lazy(() =>
  import(/*webpackChunkName: "lazyMenu"*/ "./containers/Menu")
);

function SuspenseFwdAuth({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyFwdAuth {...props} />
    </Suspense>
  );
}

function SuspenseMain({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyMain {...props} />
    </Suspense>
  );
}

function SuspenseMenu({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyMenu {...props} pageWrapId="main" outerContainerId="root" />
    </Suspense>
  );
}

const Routes = () => (
  <>
    <GlobalStyle />
    <PublicWs />
    <PrivateWs />
    <BrowserRouter>
      <Route path="/" component={SuspenseMenu} />
      <Switch>
        <Route path="/auth/callback/" component={SuspenseFwdAuth} />
        <Route path="/" component={SuspenseMain} />
        <Route component={NoMatch} />
      </Switch>
    </BrowserRouter>
  </>
);

export default Routes;
